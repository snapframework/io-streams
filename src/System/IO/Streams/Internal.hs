-- | Internal implementation of the @io-streams@ library, intended for library
-- writers
--
-- Library users should use the interface provided by "System.IO.Streams"

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module System.IO.Streams.Internal
  ( -- * Types
    SP(..)
  , StreamPair

    -- * About pushback
    -- $pushback

    -- * Input and output streams
  , InputStream(..)
  , OutputStream(..)

    -- * Primitive stream operations
  , read
  , unRead
  , peek
  , write
  , writeTo
  , atEOF

    -- * Building streams
  , makeInputStream
  , makeOutputStream
  , appendInputStream
  , concatInputStreams

    -- * Connecting streams
  , connect
  , connectTo
  , supply
  , supplyTo

    -- * Thread safety
  , lockingInputStream
  , lockingOutputStream

    -- * Utility streams
  , nullInput
  , nullOutput

    -- * Generator monad
  , Generator
  , fromGenerator
  , yield

    -- * Consumer monad
  , Consumer
  , fromConsumer
  , await
  ) where

------------------------------------------------------------------------------
import           Control.Applicative      (Applicative (..), (<$>))
import           Control.Concurrent       (newMVar, withMVar)
import           Control.Exception        (throwIO)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe   as S
import           Data.IORef               (newIORef, readIORef, writeIORef)
import           Data.Maybe               (isNothing)
import           Data.Typeable            (Typeable)
import           Data.Word                (Word8)
import           Foreign.Marshal.Utils    (copyBytes)
import           Foreign.Ptr              (castPtr)
import qualified GHC.IO.Buffer            as H
import qualified GHC.IO.BufferedIO        as H
import qualified GHC.IO.Device            as H
import           GHC.IO.Exception         (unsupportedOperation)
import           Prelude                  hiding (read)


------------------------------------------------------------------------------
-- | A strict pair type.
data SP a b = SP !a !b
  deriving (Typeable)


------------------------------------------------------------------------------
-- | An 'InputStream' generates values of type @c@ in the 'IO' monad.
--
--  Two primitive operations are defined on 'InputStream':
--
-- * @'read' :: 'InputStream' c -> 'IO' ('Maybe' c)@ reads a value from the stream,
-- where \"end of stream\" is signaled by 'read' returning 'Nothing'.
--
-- * @'unRead' :: c -> 'InputStream' c -> 'IO' ()@ \"pushes back\" a value to the
-- stream.
--
-- It is intended that 'InputStream's obey the following law:
--
-- @'unRead' c stream >> 'read' stream === 'return' ('Just' c)@
--
data InputStream a = InputStream {
      _read   :: IO (Maybe a)
    , _unRead :: a -> IO ()
    } deriving (Typeable)


------------------------------------------------------------------------------
-- | An 'OutputStream' consumes values of type @c@ in the 'IO' monad.
-- The only primitive operation defined on 'OutputStream' is:
--
-- * @'write' :: 'Maybe' c -> 'OutputStream' c -> 'IO' ()@
--
-- Values of type @c@ are written in an 'OutputStream' by wrapping them in
-- 'Just', and the end of the stream is indicated by supplying 'Nothing'.
--
-- If you supply a value after a 'Nothing', the behavior is defined by the
-- implementer of the given 'OutputStream'. (All 'OutputStream' definitions in
-- this library will simply discard the extra input.)
--
data OutputStream a = OutputStream {
      _write :: Maybe a -> IO ()
    } deriving (Typeable)


------------------------------------------------------------------------------
-- | Reads one value from an 'InputStream'.
--
-- Returns either a value wrapped in a 'Just', or 'Nothing' if the end of the
-- stream is reached.
read :: InputStream a -> IO (Maybe a)
read = _read
{-# INLINE read #-}


------------------------------------------------------------------------------
-- | Feeds a value to an 'OutputStream'. Values of type @c@ are written in an
-- 'OutputStream' by wrapping them in 'Just', and the end of the stream is
-- indicated by supplying 'Nothing'.
--
write :: Maybe a -> OutputStream a -> IO ()
write = flip _write
{-# INLINE write #-}


------------------------------------------------------------------------------
-- | Flipped version of 'write'.
--
-- /Since: 1.3.0.0./
writeTo :: OutputStream a -> Maybe a -> IO ()
writeTo = _write
{-# INLINE writeTo #-}


------------------------------------------------------------------------------
-- | Observes the first value from an 'InputStream' without consuming it.
--
-- Returns 'Nothing' if the 'InputStream' is empty. 'peek' satisfies the
-- following law:
--
-- @
-- Streams.'peek' stream >> Streams.'read' stream === Streams.'read' stream
-- @
peek :: InputStream a -> IO (Maybe a)
peek s = do
    x <- read s
    maybe (return $! ()) (_unRead s) x
    return x


------------------------------------------------------------------------------
-- | Pushes a value back onto an input stream. 'read' and 'unRead' should
-- satisfy the following law, with the possible exception of side effects:
--
-- @
-- Streams.'unRead' c stream >> Streams.'read' stream === 'return' ('Just' c)
-- @
--
-- Note that this could be used to add values back to the stream that were not
-- originally drawn from the stream.
unRead :: a -> InputStream a -> IO ()
unRead = flip _unRead


------------------------------------------------------------------------------
-- | Connects an 'InputStream' and 'OutputStream', supplying values from the
-- 'InputStream' to the 'OutputStream', and propagating the end-of-stream
-- message from the 'InputStream' through to the 'OutputStream'.
--
-- The connection ends when the 'InputStream' yields a 'Nothing'.
connect :: InputStream a -> OutputStream a -> IO ()
connect p q = loop
  where
    loop = do
        m <- read p
        maybe (write Nothing q)
              (const $ write m q >> loop)
              m
{-# INLINE connect #-}


------------------------------------------------------------------------------
-- | The 'connectTo' function is just @'flip' 'connect'@.
--
-- Useful for writing expressions like @fromList [1,2,3] >>= connectTo foo@.
--
connectTo :: OutputStream a -> InputStream a -> IO ()
connectTo = flip connect
{-# INLINE connectTo #-}


------------------------------------------------------------------------------
-- | Connects an 'InputStream' to an 'OutputStream' without passing the
-- end-of-stream notification through to the 'OutputStream'.
--
-- Use this to supply an 'OutputStream' with multiple 'InputStream's and use
-- 'connect' for the final 'InputStream' to finalize the 'OutputStream', like
-- so:
--
-- @
-- do Streams.'supply'  input1 output
--    Streams.'supply'  input2 output
--    Streams.'connect' input3 output
-- @
--
supply :: InputStream a -> OutputStream a -> IO ()
supply p q = loop
  where
    loop = do
        m <- read p
        maybe (return $! ())
              (const $ write m q >> loop)
              m
{-# INLINE supply #-}


------------------------------------------------------------------------------
-- | 'supply' with the arguments flipped.
supplyTo :: OutputStream a -> InputStream a -> IO ()
supplyTo = flip supply
{-# INLINE supplyTo #-}


------------------------------------------------------------------------------
-- | Creates an 'InputStream' from a value-producing action.
--
-- (@makeInputStream m@) calls the action @m@ each time you request a value
-- from the 'InputStream'. The given action is extended with the default
-- pushback mechanism (see "System.IO.Streams.Internal#pushback").
makeInputStream :: IO (Maybe a) -> IO (InputStream a)
makeInputStream m = do
    doneRef <- newIORef False
    pbRef   <- newIORef []
    return $! InputStream (grab doneRef pbRef) (pb pbRef)
  where
    grab doneRef pbRef = do
        l <- readIORef pbRef
        case l of
          []     -> do done <- readIORef doneRef
                       if done
                         then return Nothing
                         else do
                             x <- m
                             when (isNothing x) $ writeIORef doneRef True
                             return x
          (x:xs) -> writeIORef pbRef xs >> (return $! Just x)

    pb ref x = readIORef ref >>= \xs -> writeIORef ref (x:xs)
{-# INLINE makeInputStream #-}


------------------------------------------------------------------------------
-- | Creates an 'OutputStream' from a value-consuming action.
--
-- (@makeOutputStream f@) runs the computation @f@ on each value fed to it.
--
-- Since version 1.2.0.0, 'makeOutputStream' also ensures that output streams
-- no longer receive data once EOF is received (i.e. you can now assume that
-- makeOutputStream will feed your function @Nothing@ at most once.)
makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)
makeOutputStream func = (OutputStream . go) <$> newIORef False
  where
    go closedRef !m = do
        closed <- readIORef closedRef
        if closed
          then return $! ()
          else do
            when (isNothing m) $ writeIORef closedRef True
            func m
{-# INLINE makeOutputStream #-}


------------------------------------------------------------------------------
-- | Converts an 'InputStream' into a thread-safe 'InputStream', at a slight
-- performance penalty.
--
-- For performance reasons, this library provides non-thread-safe streams by
-- default. Use the @locking@ functions to convert these streams into slightly
-- slower, but thread-safe, equivalents.
lockingInputStream :: InputStream a -> IO (InputStream a)
lockingInputStream s = do
    mv <- newMVar $! ()
    return $! InputStream (grab mv) (pb mv)

  where
    grab mv = withMVar mv $ const $ read s
    pb mv x = withMVar mv $ const $ unRead x s
{-# INLINE lockingInputStream #-}


------------------------------------------------------------------------------
-- | Converts an 'OutputStream' into a thread-safe 'OutputStream', at a slight
-- performance penalty.
--
-- For performance reasons, this library provides non-thread-safe streams by
-- default. Use the @locking@ functions to convert these streams into slightly
-- slower, but thread-safe, equivalents.
lockingOutputStream :: OutputStream a -> IO (OutputStream a)
lockingOutputStream s = do
    mv <- newMVar $! ()
    makeOutputStream $ f mv

  where
    f mv x = withMVar mv $ const $ write x s
{-# INLINE lockingOutputStream #-}


------------------------------------------------------------------------------
-- | An empty 'InputStream' that yields 'Nothing' immediately.
nullInput :: IO (InputStream a)
nullInput = makeInputStream $ return Nothing


------------------------------------------------------------------------------
-- | An empty 'OutputStream' that discards any input fed to it.
nullOutput :: IO (OutputStream a)
nullOutput = makeOutputStream $ const $ return $! ()


------------------------------------------------------------------------------
-- | 'appendInputStream' concatenates two 'InputStream's, analogous to ('++')
-- for lists.
--
-- The second 'InputStream' continues where the first 'InputStream' ends.
--
-- Note: values pushed back to 'appendInputStream' are not propagated to either
-- wrapped 'InputStream'.
appendInputStream :: InputStream a -> InputStream a -> IO (InputStream a)
appendInputStream s1 s2 = concatInputStreams [s1, s2]


------------------------------------------------------------------------------
-- | 'concatInputStreams' concatenates a list of 'InputStream's, analogous to
-- ('++') for lists.
--
-- Subsequent 'InputStream's continue where the previous one 'InputStream'
-- ends.
--
-- Note: values pushed back to the 'InputStream' returned by
-- 'concatInputStreams' are not propagated to any of the source
-- 'InputStream's.
concatInputStreams :: [InputStream a] -> IO (InputStream a)
concatInputStreams inputStreams = do
    ref <- newIORef inputStreams
    makeInputStream $! run ref

  where
    run ref = go
      where
        go = do
            streams <- readIORef ref
            case streams of
              []       -> return Nothing
              (s:rest) -> do
                  next <- read s
                  case next of
                    Nothing -> writeIORef ref rest >> go
                    Just _  -> return next


------------------------------------------------------------------------------
-- | Checks if an 'InputStream' is at end-of-stream.
atEOF :: InputStream a -> IO Bool
atEOF s = read s >>= maybe (return True) (\k -> unRead k s >> return False)


------------------------------------------------------------------------------
-- $pushback
-- #pushback#
--
-- Users can push a value back into an input stream using the 'unRead'
-- function. Usually this will use the default pushback mechanism which
-- provides a buffer for the stream. Some stream transformers, like
-- 'takeBytes', produce streams that send pushed-back values back to the
-- streams that they wrap. A function like 'System.IO.Streams.Combinators.map'
-- cannot do this because the types don't match up:
--
-- @
-- 'System.IO.Streams.Combinators.map' :: (a -> b) -> 'InputStream' a -> 'IO' ('InputStream' b)
-- @
--
-- A function will usually document if its pushback behaviour differs from the
-- default. No matter what the case, input streams should obey the following
-- law:
--
-- @
-- Streams.'unRead' c stream >> Streams.'read' stream === 'return' ('Just' c)
-- @


                 --------------------------------------------
                 -- Typeclass instances for Handle support --
                 --------------------------------------------

------------------------------------------------------------------------------
bUFSIZ :: Int
bUFSIZ = 32752


------------------------------------------------------------------------------
unsupported :: IO a
unsupported = throwIO unsupportedOperation


------------------------------------------------------------------------------
bufferToBS :: H.Buffer Word8 -> ByteString
bufferToBS buf = S.copy $! S.fromForeignPtr raw l sz
  where
    raw  = H.bufRaw buf
    l    = H.bufL buf
    r    = H.bufR buf
    sz   = r - l


------------------------------------------------------------------------------
instance H.RawIO (InputStream ByteString) where
    read is ptr n = read is >>= maybe (return 0) f
      where
        f s = S.unsafeUseAsCStringLen s $ \(cstr, l) -> do
                  let c = min n l
                  copyBytes ptr (castPtr cstr) c
                  return $! c

    readNonBlocking  _ _ _ = unsupported
    write            _ _ _ = unsupported
    writeNonBlocking _ _ _ = unsupported


------------------------------------------------------------------------------
instance H.RawIO (OutputStream ByteString) where
    read _ _ _             = unsupported
    readNonBlocking _ _ _  = unsupported
    write os ptr n         = S.packCStringLen (castPtr ptr, n) >>=
                             flip write os . Just
    writeNonBlocking _ _ _ = unsupported


------------------------------------------------------------------------------
-- | Internal convenience synonym for a pair of input\/output streams.
type StreamPair a = SP (InputStream a) (OutputStream a)

instance H.RawIO (StreamPair ByteString) where
    read (SP is _) ptr n   = H.read is ptr n
    readNonBlocking  _ _ _ = unsupported
    write (SP _ os) ptr n  = H.write os ptr n
    writeNonBlocking _ _ _ = unsupported


------------------------------------------------------------------------------
instance H.BufferedIO (OutputStream ByteString) where
    newBuffer !_ bs            = H.newByteBuffer bUFSIZ bs
    fillReadBuffer !_ _        = unsupported
    fillReadBuffer0 !_ _       = unsupported

    flushWriteBuffer !os !buf  = do
        write (Just $! bufferToBS buf) os
        emptyWriteBuffer buf

    flushWriteBuffer0 !os !buf = do
        let s = bufferToBS buf
        let l = S.length s
        write (Just s) os
        buf' <- emptyWriteBuffer buf
        return $! (l, buf')


------------------------------------------------------------------------------
instance H.BufferedIO (InputStream ByteString) where
    newBuffer !_ !bs        = H.newByteBuffer bUFSIZ bs
    fillReadBuffer !is !buf = H.readBuf is buf
    fillReadBuffer0 _ _    = unsupported
    flushWriteBuffer _ _   = unsupported
    flushWriteBuffer0 _ _  = unsupported


------------------------------------------------------------------------------
instance H.BufferedIO (StreamPair ByteString) where
    newBuffer !_ bs              = H.newByteBuffer bUFSIZ bs
    fillReadBuffer (SP is _)     = H.fillReadBuffer is
    fillReadBuffer0 _ _          = unsupported
    flushWriteBuffer (SP _ !os)  = H.flushWriteBuffer os
    flushWriteBuffer0 (SP _ !os) = H.flushWriteBuffer0 os


------------------------------------------------------------------------------
instance H.IODevice (OutputStream ByteString) where
  ready _ _ _ = return True
  close       = write Nothing
  devType _   = return H.Stream


------------------------------------------------------------------------------
instance H.IODevice (InputStream ByteString) where
  ready _ _ _ = return True
  close _     = return $! ()
  devType _   = return H.Stream


------------------------------------------------------------------------------
instance H.IODevice (StreamPair ByteString) where
  ready _ _ _     = return True
  close (SP _ os) = write Nothing os
  devType _       = return H.Stream


------------------------------------------------------------------------------
emptyWriteBuffer :: H.Buffer Word8
                 -> IO (H.Buffer Word8)
emptyWriteBuffer buf
    = return buf { H.bufL=0, H.bufR=0, H.bufState = H.WriteBuffer }


------------------------------------------------------------------------------
-- | A 'Generator' is a coroutine monad that can be used to define complex
-- 'InputStream's. You can cause a value of type @Just r@ to appear when the
-- 'InputStream' is read by calling 'yield':
--
-- @
-- g :: 'Generator' Int ()
-- g = do
--     Streams.'yield' 1
--     Streams.'yield' 2
--     Streams.'yield' 3
-- @
--
-- A 'Generator' can be turned into an 'InputStream' by calling
-- 'fromGenerator':
--
-- @
-- m :: 'IO' ['Int']
-- m = Streams.'fromGenerator' g >>= Streams.'System.IO.Streams.toList'     \-\- value returned is [1,2,3]
-- @
--
-- You can perform IO by calling 'liftIO', and turn a 'Generator' into an
-- 'InputStream' with 'fromGenerator'.
--
-- As a general rule, you should not acquire resources that need to be freed
-- from a 'Generator', because there is no guarantee the coroutine continuation
-- will ever be called, nor can you catch an exception from within a
-- 'Generator'.
newtype Generator r a = Generator {
      unG :: IO (Either (SP r (Generator r a)) a)
    } deriving (Typeable)


------------------------------------------------------------------------------
generatorBind :: Generator r a -> (a -> Generator r b) -> Generator r b
generatorBind (Generator m) f = Generator (m >>= either step value)
  where
    step (SP v r) = return $! Left $! SP v (generatorBind r f)
    value = unG .  f
{-# INLINE generatorBind #-}


------------------------------------------------------------------------------
instance Monad (Generator r) where
   return = Generator . return . Right
   (>>=)  = generatorBind


------------------------------------------------------------------------------
instance MonadIO (Generator r) where
    liftIO = Generator . (Right `fmap`)


------------------------------------------------------------------------------
instance Functor (Generator r) where
    fmap f (Generator m) = Generator $ m >>= either step value
      where
        step (SP v m') = return $! Left $! SP v (fmap f m')
        value v        = return $! Right $! f v


------------------------------------------------------------------------------
instance Applicative (Generator r) where
    pure = Generator . return . Right

    m <*> n = do
        f <- m
        v <- n
        return $! f v


------------------------------------------------------------------------------
-- | Calling @'yield' x@ causes the value @'Just' x@ to appear on the input
-- when this generator is converted to an 'InputStream'. The rest of the
-- computation after the call to 'yield' is resumed later when the
-- 'InputStream' is 'read' again.
yield :: r -> Generator r ()
yield x = Generator $! return $! Left $! SP x (return $! ())


------------------------------------------------------------------------------
-- | Turns a 'Generator' into an 'InputStream'.
fromGenerator :: Generator r a -> IO (InputStream r)
fromGenerator (Generator m) = do
    ref <- newIORef m
    makeInputStream $! go ref
  where
    go ref = readIORef ref >>= (\n -> n >>= either step finish)
      where
        step (SP v gen) = do
            writeIORef ref $! unG gen
            return $! Just v

        finish _ = return Nothing


------------------------------------------------------------------------------
newtype Consumer c a = Consumer {
      unC :: IO (Either (Maybe c -> Consumer c a) a)
    } deriving (Typeable)


------------------------------------------------------------------------------
instance Monad (Consumer c) where
    return = Consumer . return . Right

    (Consumer m) >>= f = Consumer $ m >>= either step value
      where
        step g  = return $! Left $! (>>= f) . g
        value v = unC $ f v


------------------------------------------------------------------------------
instance MonadIO (Consumer c) where
    liftIO = Consumer . fmap Right


------------------------------------------------------------------------------
instance Functor (Consumer r) where
    fmap f (Consumer m) = Consumer (m >>= either step value)
      where
        step g = return $! Left $! (fmap f) . g
        value v = return $! Right $! f v


------------------------------------------------------------------------------
instance Applicative (Consumer r) where
    pure = return

    m <*> n = do
        f <- m
        v <- n
        return $! f v


------------------------------------------------------------------------------
await :: Consumer r (Maybe r)
await = Consumer $ return (Left return)


------------------------------------------------------------------------------
fromConsumer :: Consumer r a -> IO (OutputStream r)
fromConsumer c0 = newIORef c0 >>= makeOutputStream . go
  where
    go ref mb = do
        c  <- readIORef ref
        c' <- unC c >>= either step (const $! return c)
        writeIORef ref c'
      where
        force c = do e <- unC c
                     return $! Consumer $! return e
        step g  = force $! g mb

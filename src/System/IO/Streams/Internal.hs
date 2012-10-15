-- | Internal implementation of the @io-streams@ library, intended for library
-- writers
--
-- Library users should use the interface provided by "System.IO.Streams"

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module System.IO.Streams.Internal
  ( -- * Types
    SP(..)
  , Source(..)
  , Sink(..)
    -- * Source concatenation
  , appendSource
  , concatSources
    -- * Default sources and sinks
  , defaultPushback
  , withDefaultPushback
  , nullSource
  , nullSink
  , singletonSource
    -- * Input and output streams
  , InputStream(..)
  , OutputStream(..)
    -- * Primitive stream operations
  , read
  , unRead
  , peek
  , write
  , atEOF
    -- * Build streams
  , sourceToStream
  , sinkToStream
  , makeInputStream
  , makeOutputStream
  , appendInputStream
    -- * Connect streams
  , connect
  , connectTo
  , connectWithoutEof
  , connectToWithoutEof
    -- * Thread safety
  , lockingInputStream
  , lockingOutputStream
    -- * Utility streams
  , nullInput
  , nullOutput
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent  ( newMVar, withMVar )
import           Control.Monad       ( liftM )
import           Data.IORef          ( IORef, newIORef, readIORef, writeIORef )
import           Data.List           ( foldl' )
import           Prelude hiding      ( read )


------------------------------------------------------------------------------
-- | A strict pair type.
data SP a b = SP !a !b

------------------------------------------------------------------------------
-- TODO: Define the rest of the laws, which are basically the State monad laws
-- | A 'Source' generates values of type @c@ in the 'IO' monad.
--
-- 'Source's wrap ordinary values in a 'Just' and signal termination by yielding-- a 'Nothing'.
--
-- All 'Source's define an optional push-back mechanism.  You can assume that:
--
-- > pushback source c >>= produce = return (source, Just c)
--
-- ... unless a 'Source' documents otherwise.
--
-- Library users should use 'InputStream's, which prevent reuse of previous
-- 'Source's.
data Source c = Source {
      produce  :: IO (SP (Source c) (Maybe c))
    , pushback :: c -> IO (Source c)
    }


-- | A 'Sink' consumes values of type @c@ in the 'IO' monad.
--
-- * Supply ordinary values by wrapping them in a 'Just'
--
-- * Indicate the end of the stream by supplying a 'Nothing'
--
-- If you supply a value after a 'Nothing', the behavior is undefined.
--
-- Library users should use 'OutputStream's, which prevent reuse of previous
-- 'Sink's.
data Sink c = Sink {
      consume :: Maybe c -> IO (Sink c)
    }
{- TODO: Define the behavior when you:

    * supply multiple 'Nothing's, or

    * supply a 'Nothing' followed by a 'Just'
-}


------------------------------------------------------------------------------
-- | 'appendSource' concatenates two 'Source's, analogous to ('++') for lists.
--
-- The second 'Source' continues where the first 'Source' ends.
--
-- 'appendSource' defines a monoid with 'nullSource' as the identity:
--
-- > nullSource `appendSource` s = s
-- >
-- > s `appendSource` nullSource = s
-- >
-- >  (s1 `appendSource` s2) `appendSource` s3
-- > = s1 `appendSource` (s2 `appendSource` s3)
appendSource :: Source c -> Source c -> Source c
p `appendSource` q = Source prod pb
  where
    prod = do
        (SP p' c) <- produce p
        maybe (produce q)
              (const $ return $! SP (p' `appendSource` q) c)
              c

    pb c = do
        s' <- pushback p c
        return $! s' `appendSource` q


------------------------------------------------------------------------------
-- | 'concatSources' concatenates a list of sources, analogous to 'concat' for
-- lists.  -}
concatSources :: [Source c] -> Source c
concatSources = foldl' appendSource nullSource


{- TODO: Define better convenience functions for pushback.  These convenience
         functions still require that the user ties the knot to correctly define
         pushback, which is error-prone for non-trivial pushback
         customizations. -}
------------------------------------------------------------------------------
{- TODO: Leaving this undocumented for now since it has a very narrow use case
         and might be worth replacing with a more useful function -}
defaultPushback :: Source c -> c -> IO (Source c)
defaultPushback s c = let s' = Source { produce  = return $! SP s (Just c)
                                      , pushback = defaultPushback s'
                                      }
                      in return $! s'


------------------------------------------------------------------------------
{- TODO: Leaving this undocumented for now since it has a very narrow use case
         and might be worth replacing with a more useful function -}
withDefaultPushback :: IO (SP (Source c) (Maybe c)) -> Source c
withDefaultPushback prod = let s = Source prod (defaultPushback s)
                           in s


------------------------------------------------------------------------------
-- | An empty source that immediately yields 'Nothing'
nullSource :: Source c
nullSource = withDefaultPushback (return $! SP nullSource Nothing)


------------------------------------------------------------------------------
-- | 'nullSink' discards all values it consumes
nullSink :: Sink c
nullSink = Sink $ const $ return nullSink


------------------------------------------------------------------------------
-- | Transform any value into a 1-element 'Source'
singletonSource :: c -> Source c
singletonSource c = withDefaultPushback $ return $! SP nullSource (Just c)


------------------------------------------------------------------------------
-- A modifyMVar takes about 35ns to run on my Macbook, and the equivalent
-- readIORef/writeIORef pair takes 6ns.
--
-- Given that we'll be composing these often, we'll give up thread safety in
-- order to gain a 6x performance improvement. If you want thread-safe access
-- to a stream, you can use lockingInputStream or lockingOutputStream.

--newtype InputStream  c = IS (MVar (Source c))
--newtype OutputStream c = OS (MVar (Sink   c))

-- TODO(gdc): IORef obviously faster here, but lose thread safety. Decide what
-- to do based on benchmark data. If MVar is not appreciably slower, it should
-- be wiser to go with that.

-- | 'InputStream' protects a 'Source' by preventing old 'Source' states from
-- being reused. -}
newtype InputStream  c = IS (IORef (Source c))

-- | 'OutputStream' protects a 'Sink' by preventing old 'Sink' states from being
-- reused.
newtype OutputStream c = OS (IORef (Sink   c))

------------------------------------------------------------------------------
-- | Read one value from an 'InputStream'
--
-- Returns a value wrapped in a 'Just' or returns 'Nothing' if the stream is
-- empty.
read :: InputStream c -> IO (Maybe c)
read (IS ref) = do
    m       <- readIORef ref
    SP m' x <- produce m
    writeIORef ref m'
    return x
{-# INLINE read #-}


------------------------------------------------------------------------------
-- | Push back a value onto an input stream.
unRead :: c -> InputStream c -> IO ()
unRead c (IS ref) = readIORef ref >>= f >>= writeIORef ref
  where
    f (Source _ pb) = pb c
{-# INLINE unRead #-}
{- TODO: This actually can be used to add values back that were not drawn from
         the stream.  Perhaps the documentation should provide guidance as to
         whether or not users should do that. -}


------------------------------------------------------------------------------
-- | Wrap a 'Source' in an 'InputStream' ensuring that it can only be traversed
-- once
sourceToStream :: Source a -> IO (InputStream a)
sourceToStream = liftM IS . newIORef
{-# INLINE sourceToStream #-}


------------------------------------------------------------------------------
-- | Wrap a 'Sink' in an 'OutputStream' ensuring that it can only be traversed
-- once
sinkToStream :: Sink a -> IO (OutputStream a)
sinkToStream = liftM OS . newIORef
{-# INLINE sinkToStream #-}


------------------------------------------------------------------------------
-- | 'appendInputStream' concatenates two 'InputStream's, analogous to ('++')
-- for lists.
--
-- The second 'InputStream' continues where the first 'InputStream' ends.
--
-- Note: 'appendInputStream' does not push back to either input stream
appendInputStream :: InputStream a -> InputStream a -> IO (InputStream a)
appendInputStream s1 s2 = sourceToStream src1
  where
    src1 = withDefaultPushback read1
    src2 = withDefaultPushback read2

    read1 = do
        x <- read s1
        maybe read2 (const $! return $! SP src1 x) x

    read2 = do
        x <- read s2
        return $! SP src2 x


------------------------------------------------------------------------------
-- | Observe the first value from an 'InputStream' without consuming it
--
-- Returns 'Nothing' if the 'InputStream' is empty -}
peek :: InputStream c -> IO (Maybe c)
peek s = do
    x <- read s
    maybe (return $! ()) (\c -> unRead c s) x
    return x
{-# INLINE peek #-}


------------------------------------------------------------------------------
-- | Feed an 'OutputStream'
--
-- * Supply additional values using 'Just'
--
-- * Signal termination using 'Nothing'
write :: Maybe c -> OutputStream c -> IO ()
write c (OS ref) = readIORef ref >>= (($ c) . consume) >>= writeIORef ref
{-# INLINE write #-}


------------------------------------------------------------------------------
-- | Connect an 'InputStream' and 'OutputStream', supplying values from the
-- 'InputStream' to the 'OutputStream'
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


------------------------------------------------------------------------------
-- | Connect an 'InputStream' and 'OutputStream' without signaling termination
-- to the 'OutputStream'
--
-- Use this to supply an 'OutputStream' with multiple 'InputStream's and use
-- 'connect' for the final 'InputStream' to finalize the 'OutputStream', like
-- so:
--
-- > do connectWithoutEof input1 output
-- >    connectWithoutEof input2 output
-- >    connect           input3 output
connectWithoutEof :: InputStream a -> OutputStream a -> IO ()
connectWithoutEof p q = loop
  where
    loop = do
        m <- read p
        maybe (return $! ())
              (const $ write m q >> loop)
              m
{-# INLINE connectWithoutEof #-}


------------------------------------------------------------------------------
-- | 'connectWithoutEof' with the arguments flipped
connectToWithoutEof :: OutputStream a -> InputStream a -> IO ()
connectToWithoutEof = flip connectWithoutEof


------------------------------------------------------------------------------
-- | Create an 'InputStream' from a value-producing action
--
-- (@makeInputStream m@) calls the action @m@ each time you request a value
-- from the 'InputStream'. -}
makeInputStream :: IO (Maybe a) -> IO (InputStream a)
makeInputStream m = sourceToStream s
  where
    s = Source { produce = do
                     x <- m
                     return $! SP s x
               , pushback = defaultPushback s
               }
{-# INLINE makeInputStream #-}


------------------------------------------------------------------------------
-- | Create an 'OutputStream' from a value-consuming action
--
-- (@makeOutputStream f@) runs the function @f@ on each value fed to it.
makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)
makeOutputStream f = sinkToStream s
  where
    s = Sink (\x -> f x >> return s)
{-# INLINE makeOutputStream #-}


------------------------------------------------------------------------------
-- | Convert an 'InputStream' into a thread-safe 'InputStream', at a slight
-- performance penalty
--
-- This library provides non-thread-safe streams by default, which allow
-- faster access.  Use the @locking@ functions to convert these streams into
-- slightly slower thread-safe equivalents.
-- TODO: Perhaps distinguish the two types of input streams using types?
lockingInputStream :: InputStream a -> IO (InputStream a)
lockingInputStream s = do
    mv <- newMVar $! ()
    let src = Source { produce = withMVar mv $ const $ do
                           x <- read s
                           return $! SP src x
                     , pushback = \c -> withMVar mv $ const $ do
                                      unRead c s
                                      return src
                     }
    sourceToStream src
{-# INLINE lockingInputStream #-}


------------------------------------------------------------------------------
-- | Convert an 'OutputStream' into a thread-safe 'OutputStream', at a slight
-- performance penalty
--
-- This library provides non-thread-safe streams by default, which allow
-- faster access.  Use the @locking@ functions to convert these streams into
-- slightly slower thread-safe equivalents.
lockingOutputStream :: OutputStream a -> IO (OutputStream a)
lockingOutputStream s = do
    mv <- newMVar $! ()
    makeOutputStream $ f mv

  where
    f mv x = withMVar mv $ const $ write x s
{-# INLINE lockingOutputStream #-}


------------------------------------------------------------------------------
-- | An empty 'InputStream' that yields 'Nothing' immediately
nullInput :: IO (InputStream a)
nullInput = sourceToStream nullSource


------------------------------------------------------------------------------
-- | An empty 'OutputStream' that does nothing
nullOutput :: IO (OutputStream a)
nullOutput = sinkToStream nullSink


------------------------------------------------------------------------------
-- | Check if an 'InputStream' is exhausted
atEOF :: InputStream a -> IO Bool
atEOF s = read s >>= maybe (return True) (\k -> unRead k s >> return False)

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module System.IO.Streams.Internal
  ( Source(..)
  , Sink(..)
  , SP(..)
  , appendSource
  , concatSources
  , defaultPushback
  , withDefaultPushback
  , nullSource
  , nullSink
  , singletonSource
  , InputStream(..)
  , OutputStream(..)
  , read
  , unRead
  , write
  , sourceToStream
  , sinkToStream
  , peek
  , connect
  , connectTo
  , connectWithoutEof
  , connectToWithoutEof
  , makeInputStream
  , makeOutputStream
  , lockingInputStream
  , lockingOutputStream
  , nullInput
  , nullOutput
  , atEOF
  ) where

------------------------------------------------------------------------------
import           Control.Concurrent ( newMVar, withMVar )
import           Control.Monad      ( liftM )
import           Data.IORef         ( IORef, newIORef, readIORef, writeIORef )
import           Data.List          ( foldl' )
import           Prelude hiding     ( read )


------------------------------------------------------------------------------
-- | A strict pair type.
data SP a b = SP !a !b


------------------------------------------------------------------------------
data Source c = Source {
      produce  :: IO (SP (Source c) (Maybe c))
    , pushback :: c -> IO (Source c)
    }


data Sink c = Sink {
      consume :: Maybe c -> IO (Sink c)
    }


------------------------------------------------------------------------------
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
concatSources :: [Source c] -> Source c
concatSources = foldl' appendSource nullSource


------------------------------------------------------------------------------
defaultPushback :: Source c -> c -> IO (Source c)
defaultPushback s c = let s' = Source { produce  = return $! SP s (Just c)
                                      , pushback = defaultPushback s'
                                      }
                      in return $! s'


------------------------------------------------------------------------------
withDefaultPushback :: IO (SP (Source c) (Maybe c)) -> Source c
withDefaultPushback prod = let s = Source prod (defaultPushback s)
                           in s


------------------------------------------------------------------------------
nullSource :: Source c
nullSource = withDefaultPushback (return $! SP nullSource Nothing)


------------------------------------------------------------------------------
nullSink :: Sink c
nullSink = Sink $ const $ return nullSink


------------------------------------------------------------------------------
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

newtype InputStream  c = IS (IORef (Source c))
newtype OutputStream c = OS (IORef (Sink   c))

------------------------------------------------------------------------------
read :: InputStream c -> IO (Maybe c)
read (IS ref) = do
    m       <- readIORef ref
    SP m' x <- produce m
    writeIORef ref m'
    return x
{-# INLINE read #-}


------------------------------------------------------------------------------
unRead :: c -> InputStream c -> IO ()
unRead c (IS ref) = readIORef ref >>= f >>= writeIORef ref
  where
    f (Source _ pb) = pb c
{-# INLINE unRead #-}


------------------------------------------------------------------------------
sourceToStream :: Source a -> IO (InputStream a)
sourceToStream = liftM IS . newIORef
{-# INLINE sourceToStream #-}


------------------------------------------------------------------------------
sinkToStream :: Sink a -> IO (OutputStream a)
sinkToStream = liftM OS . newIORef
{-# INLINE sinkToStream #-}


------------------------------------------------------------------------------
peek :: InputStream c -> IO (Maybe c)
peek s = do
    x <- read s
    maybe (return $! ()) (\c -> unRead c s) x
    return x
{-# INLINE peek #-}


------------------------------------------------------------------------------
write :: Maybe c -> OutputStream c -> IO ()
write c (OS ref) = readIORef ref >>= (($ c) . consume) >>= writeIORef ref
{-# INLINE write #-}


------------------------------------------------------------------------------
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
-- Fixme: this function name sucks.
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
connectToWithoutEof :: OutputStream a -> InputStream a -> IO ()
connectToWithoutEof = flip connectWithoutEof


------------------------------------------------------------------------------
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
makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)
makeOutputStream f = sinkToStream s
  where
    s = Sink (\x -> f x >> return s)
{-# INLINE makeOutputStream #-}


------------------------------------------------------------------------------
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
lockingOutputStream :: OutputStream a -> IO (OutputStream a)
lockingOutputStream s = do
    mv <- newMVar $! ()
    makeOutputStream $ f mv

  where
    f mv x = withMVar mv $ const $ write x s
{-# INLINE lockingOutputStream #-}


------------------------------------------------------------------------------
nullInput :: IO (InputStream a)
nullInput = sourceToStream nullSource


------------------------------------------------------------------------------
nullOutput :: IO (OutputStream a)
nullOutput = sinkToStream nullSink


------------------------------------------------------------------------------
atEOF :: InputStream a -> IO Bool
atEOF s = read s >>= maybe (return True) (\k -> unRead k s >> return False)

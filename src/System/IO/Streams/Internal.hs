{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module System.IO.Streams.Internal where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Data.Functor.Contravariant
import           Data.IORef
import           Data.Monoid
import           Prelude hiding (read)


------------------------------------------------------------------------------
data Source c = Source {
      produce :: IO (Source c, Maybe c)
    }


data Sink c = Sink {
      consume :: Maybe c -> IO (Sink c)
    }


------------------------------------------------------------------------------
instance Functor Source where
    fmap f (Source p) = Source $ do
                              (q, c) <- p
                              return (fmap f q, fmap f c)
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
instance Contravariant Sink where
    contramap f (Sink p) = Sink $ \c -> do
                                q <- p $ fmap f c
                                return $ contramap f q
    {-# INLINE contramap #-}

------------------------------------------------------------------------------
instance Monoid (Source c) where
    mempty = nullSource

    p `mappend` q = Source $ do
                        (p', c) <- produce p
                        maybe (produce q)
                              (const $ return (p' `mappend` q, c))
                              c

------------------------------------------------------------------------------
nullSource :: Source c
nullSource = Source $ return (nullSource, Nothing)

nullSink :: Sink c
nullSink = Sink $ const $ return nullSink

singletonSource :: c -> Source c
singletonSource c = Source (return (nullSource, Just c))


------------------------------------------------------------------------------
-- A modifyIORef takes about 35ns to run on my Macbook, and the equivalent
-- readIORef/writeIORef pair takes 6ns.
--
-- Given that we'll be composing these, we'll give up thread safety in order to
-- gain a 6x performance improvement. If you want thread-safe access to a
-- stream, you can use lockingInputStream or lockingOutputStream.

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
    m      <- readIORef ref
    (m',x) <- produce m
    writeIORef ref m'
    return x
{-# INLINE read #-}


------------------------------------------------------------------------------
unRead :: c -> InputStream c -> IO ()
unRead c (IS ref) = readIORef ref >>= f >>= writeIORef ref
  where
    f p = return $ Source $ return (p, Just c)
{-# INLINE unRead #-}


------------------------------------------------------------------------------
peek :: InputStream c -> IO (Maybe c)
peek s = do
    x <- read s
    maybe (return ()) (\c -> unRead c s) x
    return x
{-# INLINE peek #-}


------------------------------------------------------------------------------
write :: Maybe c -> OutputStream c -> IO ()
write c (OS ref) = readIORef ref >>= (($ c) . consume) >>= writeIORef ref
{-# INLINE write #-}


------------------------------------------------------------------------------
sourceToStream :: Source a -> IO (InputStream a)
sourceToStream = liftM IS . newIORef
{-# INLINE sourceToStream #-}


sinkToStream :: Sink a -> IO (OutputStream a)
sinkToStream = liftM OS . newIORef
{-# INLINE sinkToStream #-}


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
    s = Source $ do
        x <- m
        return (s, x)
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
    makeInputStream $ f mv

  where
    f mv = withMVar mv $ const $ read s
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

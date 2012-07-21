{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Types where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Monad
import           Data.Functor.Contravariant
import           Data.Monoid
import           Prelude hiding (read)


------------------------------------------------------------------------------
data Chunk c = Chunk c | EOF
  deriving (Show)

instance Functor Chunk where
    fmap _ EOF       = EOF
    fmap f (Chunk c) = Chunk (f c)


------------------------------------------------------------------------------
chunkToMaybe :: Chunk a -> Maybe a
chunkToMaybe (Chunk c) = Just c
chunkToMaybe EOF       = Nothing


------------------------------------------------------------------------------
maybeToChunk :: Maybe a -> Chunk a
maybeToChunk (Just c) = Chunk c
maybeToChunk Nothing  = EOF


------------------------------------------------------------------------------
unchunk :: b -> (a -> b) -> Chunk a -> b
unchunk b _ EOF       = b
unchunk _ f (Chunk c) = f c


------------------------------------------------------------------------------
data Source c = Source {
      produce :: IO (Source c, Chunk c)
    }


data Sink c = Sink {
      consume :: Chunk c -> IO (Sink c)
    }


------------------------------------------------------------------------------
instance Functor Source where
    fmap f (Source p) = Source $ do
                              (q, c) <- p
                              return (fmap f q, fmap f c)


------------------------------------------------------------------------------
instance Contravariant Sink where
    contramap f (Sink p) = Sink $ \c -> do
                                    q <- p $ fmap f c
                                    return $ contramap f q


------------------------------------------------------------------------------
instance Monoid (Source c) where
    mempty = nullSource

    p `mappend` q = Source $ do
                        (p', c) <- produce p
                        unchunk (produce q)
                              (const $ return (p' `mappend` q, c))
                              c

------------------------------------------------------------------------------
nullSource :: Source c
nullSource = Source $ return (nullSource, EOF)

nullSink :: Sink c
nullSink = Sink $ const $ return nullSink


------------------------------------------------------------------------------
produceList :: [c] -> IO (InputStream c)
produceList = sourceToStream . f
  where
    f []     = Source $ return (nullSource, EOF)
    f (x:xs) = Source $ return (f xs, Chunk x)


------------------------------------------------------------------------------
listSink :: IO (OutputStream c, IO [c])
listSink = do
    r <- newMVar id
    c <- sinkToStream $ consumer r
    return (c, flush r)

  where
    consumer r = Sink $ unchunk (return nullSink)
                                  (\c -> do
                                       modifyMVar_ r $ \dl -> return (dl . (c:))
                                       return $ consumer r)

    flush r = modifyMVar r $ \dl -> return (id, dl [])


------------------------------------------------------------------------------
newtype InputStream  c = IS (MVar (Source c))
newtype OutputStream c = OS (MVar (Sink   c))


------------------------------------------------------------------------------
read :: InputStream c -> IO (Maybe c)
read (IS mv) = modifyMVar mv (liftM (fmap chunkToMaybe) . produce)


------------------------------------------------------------------------------
unRead :: c -> InputStream c -> IO ()
unRead c (IS mv) = modifyMVar_ mv f
  where
    f p = return $ Source $ return (p, Chunk c)


------------------------------------------------------------------------------
write :: Maybe c -> OutputStream c -> IO ()
write c (OS mv) = modifyMVar_ mv (($ maybeToChunk c) . consume)


------------------------------------------------------------------------------
sourceToStream :: Source a -> IO (InputStream a)
sourceToStream = liftM IS . newMVar

sinkToStream :: Sink a -> IO (OutputStream a)
sinkToStream = liftM OS . newMVar


------------------------------------------------------------------------------
connect :: InputStream a -> OutputStream a -> IO ()
connect p q = loop
  where
    loop = do
        m <- read p
        maybe (write Nothing q)
              (const $ write m q >> loop)
              m


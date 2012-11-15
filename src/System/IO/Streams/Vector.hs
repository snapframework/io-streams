{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module System.IO.Streams.Vector
 ( -- * Vector conversions
   fromVector
 , toVector
 , outputToVector
 , writeVector

   -- * Utility
 , chunkVector
 , vectorOutputStream
 ) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar           (modifyMVar, modifyMVar_,
                                                    newMVar)
import           Control.Monad.IO.Class            (MonadIO (..))
import qualified Data.Vector.Fusion.Stream         as VS
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Data.Vector.Generic               (Vector (..))
import qualified Data.Vector.Generic               as V
import           Data.Vector.Generic.Mutable       as VM
import           System.IO.Streams.Internal        (InputStream, OutputStream,
                                                    Sink (..), fromGenerator,
                                                    nullSink, sinkToStream,
                                                    yield)
import qualified System.IO.Streams.Internal        as S
------------------------------------------------------------------------------

fromVector :: Vector v a => v a -> IO (InputStream a)
fromVector = fromGenerator . V.mapM_ yield
{-# INLINE fromVector #-}


------------------------------------------------------------------------------
toVector :: Vector v a => InputStream a -> IO (v a)
toVector input = (VM.munstream $ SM.unfoldrM go ()) >>= V.basicUnsafeFreeze
  where
    go !z = S.read input >>= maybe (return Nothing)
                                   (\x -> return $! Just (x, z))
{-# INLINE toVector #-}


------------------------------------------------------------------------------
vectorOutputStream :: Vector v c => IO (OutputStream c, IO (v c))
vectorOutputStream = do
    r <- newMVar VS.empty
    c <- sinkToStream $ consumer r
    return (c, flush r)

  where
    consumer r = go
      where
        go = Sink $ maybe (return nullSink)
                          (\c -> do
                               modifyMVar_ r $ return . VS.cons c
                               return go)
    flush r = modifyMVar r $ \str -> return $! (VS.empty, V.unstreamR str)
{-# INLINE vectorOutputStream #-}


------------------------------------------------------------------------------
outputToVector :: Vector v a => (OutputStream a -> IO b) -> IO (v a)
outputToVector f = do
    (os, getVec) <- vectorOutputStream
    _ <- f os
    getVec
{-# INLINE outputToVector #-}


------------------------------------------------------------------------------
chunkVector :: Vector v a => Int -> InputStream a -> IO (InputStream (v a))
chunkVector n input = fromGenerator $ go n VS.empty
  where
    go !k !str | k <= 0    = yield (V.unstreamR str) >> go n VS.empty
               | otherwise = do
                     liftIO (S.read input) >>= maybe finish chunk
      where
        finish = let v = V.unstreamR str
                 in if V.null v then return $! () else yield v
        chunk x = go (k - 1) (VS.cons x str)
{-# INLINE chunkVector #-}


------------------------------------------------------------------------------
writeVector :: Vector v a => v a -> OutputStream a -> IO ()
writeVector v out = V.mapM_ (flip S.write out . Just) v

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | Vector conversions and utilities.

module System.IO.Streams.Vector
 ( -- * Vector conversions
   fromVector
 , toVector
 , toVectorSized
 , outputToVector
 , outputToVectorSized
 , toMutableVector
 , toMutableVectorSized
 , outputToMutableVector
 , outputToMutableVectorSized
 , writeVector

   -- * Utility
 , chunkVector
 , vectorOutputStream
 , vectorOutputStreamSized
 , mutableVectorOutputStream
 , mutableVectorOutputStreamSized
 ) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar     (modifyMVar, modifyMVar_,
                                              newMVar)
import           Control.Monad               (liftM, (>=>))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Primitive     (PrimState (..))
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.Vector.Generic         (Vector (..))
import qualified Data.Vector.Generic         as V
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           System.IO.Streams.Internal  (InputStream, OutputStream,
                                              Sink (..), fromGenerator,
                                              nullSink, sinkToStream, yield)
import qualified System.IO.Streams.Internal  as S


------------------------------------------------------------------------------
-- | Transforms a vector into an 'InputStream' that yields each of the values
-- in the vector in turn.
--
-- @
-- ghci> import "Control.Monad"
-- ghci> import qualified "System.IO.Streams" as Streams
-- ghci> import qualified "Data.Vector" as V
-- ghci> let v = V.'Data.Vector.fromList' [1, 2]
-- ghci> is <- Streams.'fromVector' v
-- ghci> 'Control.Monad.replicateM' 3 (Streams.'read' is)
-- ['Just' 1,'Just' 2,'Nothing']
-- @
fromVector :: Vector v a => v a -> IO (InputStream a)
fromVector = fromGenerator . V.mapM_ yield
{-# INLINE fromVector #-}


------------------------------------------------------------------------------
-- | Drains an 'InputStream', converting it to a vector. Note that this
-- function reads the entire 'InputStream' strictly into memory and as such is
-- not recommended for streaming applications or where the size of the input is
-- not bounded or known.
--
-- @
-- ghci> is <- Streams.'Streams.fromList' [(1::Int)..4]
-- ghci> Streams.'toVector' is :: 'IO' (V.'Vector' Int)
-- fromList [1,2,3,4]
-- @
toVector :: Vector v a => InputStream a -> IO (v a)
toVector = toVectorSized dEFAULT_BUFSIZ
{-# INLINE toVector #-}


------------------------------------------------------------------------------
-- | Like 'toVector', but allows control over how large the vector buffer is to
-- start with.
toVectorSized :: Vector v a => Int -> InputStream a -> IO (v a)
toVectorSized n = toMutableVectorSized n >=> V.basicUnsafeFreeze
{-# INLINE toVectorSized #-}


------------------------------------------------------------------------------
-- | Drains an 'InputStream', converting it to a mutable vector. Note that this
-- function reads the entire 'InputStream' strictly into memory and as such is
-- not recommended for streaming applications or where the size of the input is
-- not bounded or known.
toMutableVector :: VM.MVector v a => InputStream a -> IO (v (PrimState IO) a)
toMutableVector = toMutableVectorSized dEFAULT_BUFSIZ


------------------------------------------------------------------------------
-- | Like 'toMutableVector', but allows control over how large the vector
-- buffer is to start with.
toMutableVectorSized :: VM.MVector v a =>
                        Int            -- ^ initial size of the vector buffer
                     -> InputStream a
                     -> IO (v (PrimState IO) a)
toMutableVectorSized initialSize input = vfNew initialSize >>= go
  where
    go vfi = S.read input >>= maybe (vfFinish vfi) (vfAppend vfi >=> go)
{-# INLINE toMutableVectorSized #-}


------------------------------------------------------------------------------
-- | 'vectorOutputStream' returns an 'OutputStream' which stores values fed
-- into it and an action which flushes all stored values to a vector.
--
-- The flush action resets the store.
--
-- Note that this function /will/ buffer any input sent to it on the heap.
-- Please don't use this unless you're sure that the amount of input provided
-- is bounded and will fit in memory without issues.
--
-- @
-- ghci> (os, flush) <- Streams.'vectorOutputStream' :: IO ('OutputStream' Int, IO (V.'Vector' Int))
-- ghci> Streams.'Streams.write' (Just 1) os
-- ghci> Streams.'Streams.write' (Just 2) os
-- ghci> flush
-- fromList [1,2]
-- ghci> Streams.'Streams.write' (Just 3) os
-- ghci> Streams.'Streams.write' Nothing  os
-- ghci> Streams.'Streams.write' (Just 4) os
-- ghci> flush
-- fromList [3]
-- @
vectorOutputStream :: Vector v c => IO (OutputStream c, IO (v c))
vectorOutputStream = vectorOutputStreamSized dEFAULT_BUFSIZ
{-# INLINE vectorOutputStream #-}


------------------------------------------------------------------------------
-- | Like 'vectorOutputStream', but allows control over how large the vector
-- buffer is to start with.
vectorOutputStreamSized :: Vector v c => Int -> IO (OutputStream c, IO (v c))
vectorOutputStreamSized n = do
    (os, flush) <- mutableVectorOutputStreamSized n
    return $! (os, flush >>= V.basicUnsafeFreeze)


------------------------------------------------------------------------------
data VectorFillInfo v c = VectorFillInfo {
      _vec :: !(v (PrimState IO) c)
    , _idx :: {-# UNPACK #-} !(IORef Int)

    -- TODO: vector contains its own size
    , _sz  :: {-# UNPACK #-} !(IORef Int)
    }


------------------------------------------------------------------------------
vfNew :: MVector v a => Int -> IO (VectorFillInfo v a)
vfNew initialSize = do
    v  <- VM.unsafeNew initialSize
    i  <- newIORef 0
    sz <- newIORef initialSize
    return $! VectorFillInfo v i sz


------------------------------------------------------------------------------
vfFinish :: MVector v a =>
            VectorFillInfo v a
         -> IO (v (PrimState IO) a)
vfFinish vfi = liftM (flip VM.unsafeTake v) $ readIORef i
  where
    v = _vec vfi
    i = _idx vfi


------------------------------------------------------------------------------
vfAppend :: MVector v a =>
         VectorFillInfo v a
      -> a
      -> IO (VectorFillInfo v a)
vfAppend vfi !x = do
    i  <- readIORef iRef
    sz <- readIORef szRef
    if i < sz then add i else grow sz
  where
    v     = _vec vfi
    iRef  = _idx vfi
    szRef = _sz vfi

    add i = do
        VM.unsafeWrite v i x
        writeIORef iRef $! i + 1
        return vfi

    grow sz = do
        let !sz' = sz * 2
        v' <- VM.unsafeGrow v sz
        writeIORef szRef sz'
        vfAppend (vfi { _vec = v' }) x


------------------------------------------------------------------------------
-- | 'mutableVectorOutputStream' returns an 'OutputStream' which stores values
-- fed into it and an action which flushes all stored values to a vector.
--
-- The flush action resets the store.
--
-- Note that this function /will/ buffer any input sent to it on the heap.
-- Please don't use this unless you're sure that the amount of input provided
-- is bounded and will fit in memory without issues.
mutableVectorOutputStream :: VM.MVector v c =>
                             IO (OutputStream c, IO (v (PrimState IO) c))
mutableVectorOutputStream = mutableVectorOutputStreamSized dEFAULT_BUFSIZ


------------------------------------------------------------------------------
-- | Like 'mutableVectorOutputStream', but allows control over how large the
-- vector buffer is to start with.
mutableVectorOutputStreamSized :: VM.MVector v c =>
                                  Int
                               -> IO (OutputStream c, IO (v (PrimState IO) c))
mutableVectorOutputStreamSized initialSize = do
    r <- vfNew initialSize >>= newMVar
    c <- sinkToStream $ consumer r
    return (c, flush r)

  where
    consumer r = go
      where
        go = Sink $ maybe (return nullSink)
                          (\c -> do
                               modifyMVar_ r $ flip vfAppend c
                               return go)
    flush r = modifyMVar r $ \vfi -> do
                                !v   <- vfFinish vfi
                                vfi' <- vfNew initialSize
                                return $! (vfi', v)
{-# INLINE mutableVectorOutputStream #-}


------------------------------------------------------------------------------
-- | Given an IO action that requires an 'OutputStream', creates one and
-- captures all the output the action sends to it as a mutable vector.
--
-- Example:
--
-- @
-- ghci> import "Control.Applicative"
-- ghci> ('connect' \<\$\> 'System.IO.Streams.fromList' [1, 2, 3::'Int'])
--        \>\>= 'outputToMutableVector'
--        \>\>= V.'Data.Vector.freeze'
-- fromList [1,2,3]
-- @
outputToMutableVector :: MVector v a =>
                         (OutputStream a -> IO b)
                      -> IO (v (PrimState IO) a)
outputToMutableVector = outputToMutableVectorSized dEFAULT_BUFSIZ
{-# INLINE outputToMutableVector #-}


------------------------------------------------------------------------------
-- | Like 'outputToMutableVector', but allows control over how large the vector
-- buffer is to start with.
outputToMutableVectorSized :: MVector v a =>
                              Int
                           -> (OutputStream a -> IO b)
                           -> IO (v (PrimState IO) a)
outputToMutableVectorSized n f = do
    (os, getVec) <- mutableVectorOutputStreamSized n
    _ <- f os
    getVec
{-# INLINE outputToMutableVectorSized #-}


------------------------------------------------------------------------------
-- | Given an IO action that requires an 'OutputStream', creates one and
-- captures all the output the action sends to it as a vector.
--
-- Example:
--
-- @
-- ghci> (('connect' <$> 'System.IO.Streams.fromList' [1, 2, 3]) >>= 'outputToVector')
--           :: IO ('Data.Vector.Vector' Int)
-- fromList [1,2,3]
-- @
outputToVector :: Vector v a => (OutputStream a -> IO b) -> IO (v a)
outputToVector = outputToVectorSized dEFAULT_BUFSIZ
{-# INLINE outputToVector #-}


------------------------------------------------------------------------------
-- | Like 'outputToVector', but allows control over how large the vector buffer
-- is to start with.
outputToVectorSized :: Vector v a =>
                       Int
                    -> (OutputStream a -> IO b)
                    -> IO (v a)
outputToVectorSized n = outputToMutableVectorSized n >=> V.basicUnsafeFreeze
{-# INLINE outputToVectorSized #-}


------------------------------------------------------------------------------
-- | Splits an input stream into chunks of at most size @n@.
--
-- Example:
--
-- @
-- ghci> ('System.IO.Streams.fromList' [1..14::Int] >>= 'chunkVector' 4 >>= 'System.IO.Streams.toList')
--          :: IO ['Data.Vector.Vector' Int]
-- [fromList [1,2,3,4],fromList [5,6,7,8],fromList [9,10,11,12],fromList [13,14]]
-- @
chunkVector :: Vector v a => Int -> InputStream a -> IO (InputStream (v a))
chunkVector n input = if n <= 0
                        then error $ "chunkVector: bad size: " ++ show n
                        else vfNew n >>= fromGenerator . go n
  where
    doneChunk !vfi = do
        liftIO (vfFinish vfi >>= V.unsafeFreeze) >>= yield
        !vfi' <- liftIO $ vfNew n
        go n vfi'

    go !k !vfi | k <= 0    = doneChunk vfi
               | otherwise = liftIO (S.read input) >>= maybe finish chunk
      where
        finish = do
            v <- liftIO (vfFinish vfi >>= V.unsafeFreeze)
            if V.null v then return $! () else yield v

        chunk x = do
            !vfi' <- liftIO $ vfAppend vfi x
            go (k - 1) vfi'
{-# INLINE chunkVector #-}


------------------------------------------------------------------------------
-- | Feeds a vector to an 'OutputStream'. Does /not/ write an end-of-stream to
-- the stream.
--
-- @
-- ghci> let v = V.'fromList' [1..4] :: V.'Vector' Int
-- ghci> os \<- Streams.'unlines' Streams.'stdout' >>= Streams.'Streams.contramap' (S.pack . show) :: IO ('OutputStream' Int)
-- ghci> Streams.'writeVector' v os
-- 1
-- 2
-- 3
-- 4
-- @
writeVector :: Vector v a => v a -> OutputStream a -> IO ()
writeVector v out = V.mapM_ (flip S.write out . Just) v
{-# INLINE writeVector #-}


------------------------------------------------------------------------------
dEFAULT_BUFSIZ :: Int
dEFAULT_BUFSIZ = 64

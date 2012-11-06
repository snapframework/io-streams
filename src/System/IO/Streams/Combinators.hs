-- | Generic stream manipulations

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Streams.Combinators
 ( -- * Folds
   inputFoldM
 , outputFoldM

   -- * Maps
 , mapM
 , contramapM

   -- * Filter
 , filterM

   -- * Zip and unzip
 , zipM
 , unzipM

   -- * Utility
 , intercalate
 , skipToEof
 ) where

------------------------------------------------------------------------------
import Control.Concurrent.MVar    ( newMVar, withMVar )
import Control.Monad              ( liftM, when )
import Data.IORef                 ( atomicModifyIORef
                                  , modifyIORef
                                  , newIORef
                                  , readIORef
                                  , writeIORef
                                  )
import Prelude             hiding ( mapM, read )
------------------------------------------------------------------------------
import System.IO.Streams.Internal ( InputStream
                                  , OutputStream
                                  , Source(..)
                                  , SP(..)
                                  , makeInputStream
                                  , makeOutputStream
                                  , read
                                  , sourceToStream
                                  , unRead
                                  , write
                                  )

------------------------------------------------------------------------------
outputFoldM :: (a -> b -> IO a)
            -> a
            -> OutputStream b
            -> IO (OutputStream b, IO a)
outputFoldM f initial stream = do
    ref <- newIORef initial
    os  <- makeOutputStream (wr ref)
    return (os, fetch ref)

  where
    wr _ Nothing       = write Nothing stream
    wr ref mb@(Just x) = do
        !z  <- readIORef ref
        !z' <- f z x
        writeIORef ref z'
        write mb stream

    fetch ref = atomicModifyIORef ref $ \x -> (initial, x)


------------------------------------------------------------------------------
inputFoldM :: (a -> b -> IO a)
           -> a
           -> InputStream b
           -> IO (InputStream b, IO a)
inputFoldM f initial stream = do
    ref <- newIORef initial
    is  <- makeInputStream (rd ref)
    return (is, fetch ref)

  where
    twiddle _ Nothing = return Nothing

    twiddle ref mb@(Just x) = do
        !z  <- readIORef ref
        !z' <- f z x
        writeIORef ref z'
        return mb

    rd ref = read stream >>= twiddle ref

    fetch ref = atomicModifyIORef ref $ \x -> (initial, x)


------------------------------------------------------------------------------
-- | Maps an impure function over an 'InputStream'.
--
-- @mapM f s@ passes all output from @s@ through the impure function @f@.
--
-- Satisfies the following laws:
--
-- > mapM (f >=> g) = mapM f >=> mapM g
-- >
-- > mapM return = return
mapM :: (a -> IO b) -> InputStream a -> IO (InputStream b)
mapM f s = makeInputStream g
  where
    g = do
        mb <- read s >>= maybe (return Nothing)
                               (\x -> liftM Just $ f x)

        return mb


------------------------------------------------------------------------------
-- | Contravariant counterpart to 'mapM'.
--
-- (@contramapM f s@) passes all input to @s@ through the impure function @f@
--
-- Satisfies the following laws:
--
-- > contramapM (f >=> g) = contramapM g >=> contramapM f
-- >
-- > contramapM return = return
contramapM :: (a -> IO b) -> OutputStream b -> IO (OutputStream a)
contramapM f s = makeOutputStream g
  where
    g Nothing = write Nothing s

    g (Just x) = do
        !y <- f x
        write (Just y) s


------------------------------------------------------------------------------
-- | Drives an 'InputStream' to end-of-stream, discarding all of the yielded
-- values.
skipToEof :: InputStream a -> IO ()
skipToEof str = go
  where
    go = read str >>= maybe (return ()) (const go)
{-# INLINE skipToEof #-}


------------------------------------------------------------------------------
-- | Drops chunks from an input stream if they fail to match a given filter
-- predicate. See 'Prelude.filter'.
--
-- Items pushed back to the returned stream are propagated back upstream.
--
-- Example:
--
-- @
-- 'fromList' [\"the\", \"quick\", \"brown\", \"fox\"] >>=
--     'filterM' ('return' . (/= \"brown\")) >>= 'toList'
-- ghci> [\"the\",\"quick\",\"fox\"]
-- @
filterM :: (a -> IO Bool)
        -> InputStream a
        -> IO (InputStream a)
filterM p src = sourceToStream source
  where
    source = Source {
               produce  = prod
             , pushback = pb
             }

    prod = read src >>= maybe eof chunk

    chunk s = do
        b <- p s
        if b then return $! SP source (Just s)
             else prod

    eof = return $! flip SP Nothing Source {
            produce  = eof
          , pushback = pb
          }

    pb s = unRead s src >> return source


------------------------------------------------------------------------------
-- TODO: doc
--
-- Example:
--
-- @
-- ghci> is <- 'System.IO.Streams.List.fromList' [\"nom\", \"nom\", \"nom\"::'ByteString']
-- ghci> 'System.IO.Streams.List.outputToList' (\os -> 'intercalate' \"burp!\" os >>= 'System.IO.Streams.connect' is)
-- [\"nom\",\"burp!\",\"nom\",\"burp!\",\"nom\"]
-- @
intercalate :: a -> OutputStream a -> IO (OutputStream a)
intercalate sep os = newIORef False >>= makeOutputStream . f
  where
    f _ Nothing = write Nothing os
    f sendRef s    = do
        b <- readIORef sendRef
        writeIORef sendRef True
        when b $ write (Just sep) os
        write s os


------------------------------------------------------------------------------
-- | Combines two input streams. Continues yielding elements from both input
-- streams until one of them finishes.
zipM :: InputStream a -> InputStream b -> IO (InputStream (a, b))
zipM src1 src2 = makeInputStream src
  where
    src = read src1 >>= (maybe (return Nothing) $ \a ->
            read src2 >>= (maybe (unRead a src1 >> return Nothing) $ \b ->
              return $! Just $! (a, b)))


------------------------------------------------------------------------------
-- | Takes apart a stream of pairs, producing a pair of input streams. Reading
-- from either of the produced streams will cause a pair of values to be pulled
-- from the original stream if necessary. Note that reading @n@ values from one
-- of the returned streams will cause @n@ values to be buffered at the other
-- stream.
--
-- Access to the original stream is thread safe, i.e. guarded by a lock.
unzipM :: InputStream (a, b) -> IO (InputStream a, InputStream b)
unzipM os = do
    lock <- newMVar $! ()
    buf1 <- newIORef id
    buf2 <- newIORef id

    is1  <- makeInputStream $ src lock id buf1 buf2
    is2  <- makeInputStream $ src lock twist buf2 buf1

    return (is1, is2)

  where
    twist (a, b) = (b, a)

    src lock proj myBuf theirBuf = withMVar lock $ const $ do
        dl <- readIORef myBuf

        case dl [] of
          []     -> more
          (x:xs) -> writeIORef myBuf (xs++) >> (return $! Just x)
      where
        more = read os >>=
               maybe (return Nothing)
                     (\x -> do
                          let (a, b) = proj x
                          modifyIORef theirBuf (. (b:))
                          return $! Just a)

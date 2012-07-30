{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Based on Simon's blaze-bytestring-enumerator code. TODO(gdc) properly credit

module System.IO.Streams.Blaze where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Blaze.ByteString.Builder.Internal
import           Blaze.ByteString.Builder.Internal.Types
import           Blaze.ByteString.Builder.Internal.Buffer
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.List
------------------------------------------------------------------------------
import           System.IO.Streams.Internal
import           System.IO.Streams.List


------------------------------------------------------------------------------
builderStream :: OutputStream ByteString -> IO (OutputStream Builder)
builderStream = builderStreamWith (allNewBuffersStrategy defaultBufferSize)


------------------------------------------------------------------------------
unsafeBuilderStream :: IO Buffer
                    -> OutputStream ByteString
                    -> IO (OutputStream Builder)
unsafeBuilderStream = builderStreamWith . reuseBufferStrategy


------------------------------------------------------------------------------
builderStreamWith :: BufferAllocStrategy
                  -> OutputStream ByteString
                  -> IO (OutputStream Builder)
builderStreamWith (ioBuf0, nextBuf) os = do
    sinkToStream (sink ioBuf0)
  where
    sink ioBuf = Sink $ maybe eof chunk
      where
        eof = do
            buf <- ioBuf
            case unsafeFreezeNonEmptyBuffer buf of
              Nothing -> write Nothing os
              x       -> write x os >> write Nothing os

            return nullSink

        chunk c = feed (unBuilder c (buildStep finalStep)) ioBuf


    finalStep !(BufRange pf _) = return $ Done pf ()

    feed bStep ioBuf = do
        !buf   <- ioBuf
        signal <- execBuildStep bStep buf

        case signal of
          Done op' _ -> return $ sink (return (updateEndOfSlice buf op'))

          BufferFull minSize op' bStep' -> do
              let buf' = updateEndOfSlice buf op'
                  {-# INLINE cont #-}
                  cont = do
                      ioBuf' <- nextBuf minSize buf'
                      feed bStep' ioBuf'

              case unsafeFreezeNonEmptyBuffer buf' of
                Nothing -> cont
                x       -> write x os >> cont

          InsertByteString op' bs bStep' -> do
              let buf' = updateEndOfSlice buf op'

              case unsafeFreezeNonEmptyBuffer buf' of
                Nothing -> return ()
                x       -> write x os

              when (not $ S.null bs) $ write (Just bs) os

              ioBuf' <- nextBuf 1 buf'
              feed bStep' ioBuf'


------------------------------------------------------------------------------
example :: IO [ByteString]
example = do
    let l1 = intersperse " " ["the", "quick", "brown", "fox"]
    let l2 = intersperse " " ["jumped", "over", "the"]

    let l = map fromByteString l1 ++ [flush] ++ map fromByteString l2

    is <- fromList l

    (os0, grab) <- listOutputStream

    os <- builderStream os0

    connect is os >> grab


------------------------------------------------------------------------------
example2 :: IO [ByteString]
example2 = do
    let l1 = intersperse " " $ replicate 18000 "foo!"
    is <- fromList $ map fromByteString l1

    (os0, grab) <- listOutputStream
    os <- builderStream os0

    connect is os >> grab

{-# LANGUAGE BangPatterns #-}

module System.IO.Streams.ByteString
 ( countInput
 , countOutput
 , readNoMoreThan
 , writeNoMoreThan
 , MatchInfo(..)
 , boyerMooreHorspool
 ) where

------------------------------------------------------------------------------
import           Data.ByteString               (ByteString)
import qualified Data.ByteString.Char8         as S
import           Data.Int
import           Prelude                       hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Combinators
import           System.IO.Streams.Internal
import           System.IO.Streams.Internal.BoyerMooreHorspool


------------------------------------------------------------------------------
countInput :: InputStream ByteString -> IO (InputStream ByteString, IO Int64)
countInput = inputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
countOutput :: OutputStream ByteString
            -> IO (OutputStream ByteString, IO Int64)
countOutput = outputFoldM f 0
  where
    f !count s = return z
      where
        !c = S.length s
        !z = toEnum c + count


------------------------------------------------------------------------------
readNoMoreThan :: Int64
               -> InputStream ByteString
               -> IO (InputStream ByteString)
readNoMoreThan k0 src = sourceToStream $ source k0
  where
    source !k =
        Source $ read src >>= maybe (return (nullSource, Nothing)) chunk

      where
        chunk s = let l  = toEnum $ S.length s
                      k' = k - l
                  in if k' < 0
                       then let (a,b) = S.splitAt (fromEnum k) s
                            in do
                                unRead b src
                                return (nullSource, Just a)
                       else return (source k', Just s)


------------------------------------------------------------------------------
writeNoMoreThan :: Int64
                -> OutputStream ByteString
                -> IO (OutputStream ByteString)
writeNoMoreThan k0 str = sinkToStream $ sink k0
  where
    sink !k = Sink g
      where
        g Nothing     = write Nothing str >> return nullSink

        g mb@(Just x) = let l  = toEnum $ S.length x
                            k' = k - l
                        in if k' < 0
                             then let a = S.take (fromEnum k) x
                                  in write (Just a) str >> return nullSink
                             else write mb str >> return (sink k')

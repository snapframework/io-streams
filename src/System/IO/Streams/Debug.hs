{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Debug (debugByteStringInputStream) where

import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as S
import           System.IO.Streams.Internal (InputStream, OutputStream)
import qualified System.IO.Streams.Internal as Streams

debugByteStringInputStream ::
       ByteString  -- ^ name of this debug stream, will be prepended to debug
                   -- output
    -> OutputStream ByteString
                   -- ^ stream the debug info will be sent to
    -> InputStream ByteString
    -> IO (InputStream ByteString)
debugByteStringInputStream name debugStream inputStream =
    Streams.sourceToStream source

  where
    source = Streams.Source produce pb

    produce = do
        m <- Streams.read inputStream
        Streams.write (Just $ describe m) debugStream
        return $! Streams.SP source m

    pb c = Streams.unRead c inputStream >> return source

    describe m = S.concat [ name, ": got ", describeChunk m, "\n" ]

    describeChunk Nothing = "EOF"
    describeChunk (Just s) = S.concat [ "chunk of length "
                                 , S.pack $ show $ S.length s
                                 , ": "
                                 , condense s ]

    condense s | l < 32 = s
               | otherwise = S.concat [ S.take k s, "...", S.drop (l - k) s ]
      where
        k = 14
        l = S.length s

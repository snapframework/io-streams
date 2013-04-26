{-# LANGUAGE OverloadedStrings #-}

-- | Convenience module for debugging streams. Provides stream transformers
-- that wrap 'InputStream's and 'OutputStream's, sending a description of all
-- data to an 'OutputStream' for debugging.

module System.IO.Streams.Debug
 ( -- * Debuggers
   debugInput
 , debugOutput
 , debugInputBS
 , debugOutputBS
 ) where

import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Char8      as S
import           System.IO.Streams.Internal (InputStream (..), OutputStream)
import qualified System.IO.Streams.Internal as Streams

------------------------------------------------------------------------------
debugInput ::
     (a -> ByteString)         -- ^ function to convert stream elements to
                               --   'ByteString'
  -> ByteString                -- ^ name of this debug stream, will be
                               --   prepended to debug output
  -> OutputStream ByteString   -- ^ stream the debug info will be sent to
  -> InputStream a             -- ^ input stream
  -> IO (InputStream a)
debugInput toBS name debugStream inputStream = return $ InputStream produce pb
  where
    produce = do
        m <- Streams.read inputStream
        Streams.write (Just $! describe m) debugStream
        return m

    pb c = do
        let s = S.concat [name, ": pushback: ", toBS c, "\n"]
        Streams.write (Just s) debugStream
        Streams.unRead c inputStream

    describe m = S.concat [name, ": got ", describeChunk m, "\n"]

    describeChunk Nothing = "EOF"
    describeChunk (Just s) = S.concat [ "chunk: ", toBS s ]


------------------------------------------------------------------------------
debugInputBS ::
     ByteString                -- ^ name of this debug stream, will be
                               --   prepended to debug output
  -> OutputStream ByteString   -- ^ stream the debug info will be sent to
  -> InputStream ByteString    -- ^ input stream
  -> IO (InputStream ByteString)
debugInputBS = debugInput condense


------------------------------------------------------------------------------
debugOutput :: (a -> ByteString)        -- ^ function to convert stream
                                        --   elements to 'ByteString'
            -> ByteString               -- ^ name of this debug stream, will be
                                        --   prepended to debug output
            -> OutputStream ByteString  -- ^ debug stream
            -> OutputStream a           -- ^ output stream
            -> IO (OutputStream a)
debugOutput toBS name debugStream outputStream =
    Streams.makeOutputStream f
  where
    f m = do
        Streams.write (Just $ describe m) debugStream
        Streams.write m outputStream

    describe m = S.concat [name, ": got ", describeChunk m, "\n"]

    describeChunk Nothing = "EOF"
    describeChunk (Just s) = S.concat [ "chunk: ", toBS s]


------------------------------------------------------------------------------
debugOutputBS ::
     ByteString                -- ^ name of this debug stream, will be
                               --   prepended to debug output
  -> OutputStream ByteString   -- ^ stream the debug info will be sent to
  -> OutputStream ByteString    -- ^ output stream
  -> IO (OutputStream ByteString)
debugOutputBS = debugOutput condense


------------------------------------------------------------------------------
condense :: ByteString -> ByteString
condense s | l < 32 = S.concat [ "\"", s, "\"" ]
           | otherwise = S.concat [
                           "\""
                         , S.take k s
                         , " ... "
                         , S.drop (l - k) s
                         , "\" ("
                         , S.pack (show l)
                         , " bytes)"
                         ]
  where
    k = 14
    l = S.length s

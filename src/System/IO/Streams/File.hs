{-# LANGUAGE CPP #-}

module System.IO.Streams.File
  ( withFileAsInput
  , withFileAsInputStartingAt
  , withFileAsOutput
  ) where

------------------------------------------------------------------------------
import           Control.Monad              (unless)
import           Data.ByteString            (ByteString)
import           Data.Int                   (Int64)
import           System.IO
------------------------------------------------------------------------------
import           System.IO.Streams.Internal
import           System.IO.Streams.Handle

#ifndef ZPORTABLE
import           System.IO.Posix.MMap
import           System.Posix.Files
import           System.Posix.Types
#endif

------------------------------------------------------------------------------
-- FIXME: withFileAsInputStream needs more efficient versions here
--
-- 1. Like original code in Snap.Iteratee, we should mmap the file here if
--    it's small enough.
--
-- 2. We should use unbuffered IO from unix on unix platforms
--
-- 3. We need a version that enumerates from a file range
--
-- 4. Move file functions to .File

------------------------------------------------------------------------------
withFileAsInput :: FilePath                          -- ^ file to open
                -> (InputStream ByteString -> IO a)  -- ^ function to run
                -> IO a
withFileAsInput = withFileAsInputStartingAt 0


------------------------------------------------------------------------------
withFileAsInputStartingAt
    :: Int64                             -- ^ starting index to seek to
    -> FilePath                          -- ^ file to open
    -> (InputStream ByteString -> IO a)  -- ^ function to run
    -> IO a


------------------------------------------------------------------------------
withFileAsOutput
    :: FilePath                           -- ^ file to open
    -> IOMode                             -- ^ mode to write in
    -> (OutputStream ByteString -> IO a)  -- ^ function to run
    -> IO a


#ifdef ZPORTABLE
------------------------------------------------------------------------------
withFileAsInputStartingAt idx fp m = withBinaryFile fp ReadMode go
  where
    go h = do
        unless (idx == 0) $ hSeek h AbsoluteSeek $ toInteger idx
        handleToInputStream h >>= m


------------------------------------------------------------------------------
withFileAsOutput fp mode m =
    withBinaryFile fp mode ((m =<<) . handleToOutputStream)


#else
------------------------------------------------------------------------------
maxMMapFileSize :: FileOffset
maxMMapFileSize = 10485760   -- 10MB


------------------------------------------------------------------------------
tooBigForMMap :: FilePath -> IO Bool
tooBigForMMap fp = do
    stat <- getFileStatus fp
    return $! fileSize stat > maxMMapFileSize


------------------------------------------------------------------------------
withFileAsInputStartingAt idx fp m = withBinaryFile fp ReadMode go
  where
    go h = do
        unless (idx == 0) $ hSeek h AbsoluteSeek $ toInteger idx
        handleToInputStream h >>= m


------------------------------------------------------------------------------
withFileAsOutput fp mode m =
    withBinaryFile fp mode ((m =<<) . handleToOutputStream)

#endif

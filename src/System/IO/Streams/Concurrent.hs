-- | Stream utilities for working with concurrent channels.

{-# LANGUAGE BangPatterns       #-}

module System.IO.Streams.Concurrent
 ( -- * Channel conversions
   inputToChan
 , chanToInput
 , chanToOutput
 , concurrentMerge
 ) where

------------------------------------------------------------------------------
import Control.Concurrent         ( forkIO )
import Control.Concurrent.Chan    ( Chan, readChan, writeChan )
import Control.Concurrent.MVar    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception          ( mask, try, throwIO, SomeException)
import Control.Monad              ( forM_ )
import Data.Maybe                 ( isNothing )
import Prelude             hiding ( read )
------------------------------------------------------------------------------
import System.IO.Streams.Internal ( InputStream
                                  , OutputStream
                                  , SP (..)
                                  , makeInputStream
                                  , makeOutputStream
                                  , nullSource
                                  , sourceToStream
                                  , withDefaultPushback
                                  , read
                                  )

------------------------------------------------------------------------------
-- | Writes the contents of an input stream to a channel until the input stream
-- yields end-of-stream.
inputToChan :: InputStream a -> Chan (Maybe a) -> IO ()
inputToChan is ch = go
  where
    go = do
        mb <- read is
        writeChan ch mb
        maybe (return ()) (const go) mb


------------------------------------------------------------------------------
-- | Turns a 'Chan' into an input stream.
--
chanToInput :: Chan (Maybe a) -> IO (InputStream a)
chanToInput ch = sourceToStream src
  where
    src = withDefaultPushback $ do
              mb <- readChan ch
              let src' = if isNothing mb then nullSource else src
              return $! SP src' mb


------------------------------------------------------------------------------
-- | Turns a 'Chan' into an output stream.
--
chanToOutput :: Chan (Maybe a) -> IO (OutputStream a)
chanToOutput = makeOutputStream . writeChan


------------------------------------------------------------------------------
-- | Concurrently merges two 'InputStream's combining values in the order they
-- become available
--
-- Does /not/ forward end-of-stream notifications
--
-- This traps exceptions in each concurrent thread and re-raises them in the
-- current thread
concurrentMerge :: [InputStream a] -> IO (InputStream a)
concurrentMerge iss = do
    mv <- newEmptyMVar
    mask $ \restore -> forM_ iss $ \is -> forkIO $ do
        let producer = do
                emb <- try $ restore $ read is
                case emb of
                    Left exc       -> do
                        putMVar mv (Left (exc :: SomeException))
                        producer
                    Right (Just a) -> do
                        putMVar mv (Right a)
                        producer
                    Right Nothing  -> return ()
        producer
    makeInputStream $ do
        emb <- takeMVar mv
        case emb of
            Left exc -> throwIO exc
            Right a  -> return (Just a)

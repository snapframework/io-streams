-- | Stream utilities for working with concurrent channels.

{-# LANGUAGE BangPatterns       #-}

module System.IO.Streams.Concurrent
 ( inputToChan
 , chanToInput
 , chanToOutput
 ) where

------------------------------------------------------------------------------
import Control.Concurrent.Chan    ( Chan, readChan, writeChan )
import Data.Maybe                 ( isNothing )
import Prelude             hiding ( read )
------------------------------------------------------------------------------
import System.IO.Streams.Internal ( InputStream
                                  , OutputStream
                                  , SP (..)
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

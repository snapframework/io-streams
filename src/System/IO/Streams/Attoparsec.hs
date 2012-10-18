-- | This module provides support for parsing values from 'InputStream's using
-- @attoparsec@.

module System.IO.Streams.Attoparsec
  ( -- * Parsing
    ParseException(..)
  , parseFromStream
  , parserToInputStream
  ) where

import System.IO.Streams.Internal.Attoparsec

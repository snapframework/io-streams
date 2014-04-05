-- | This module provides support for parsing values from 'InputStream's using
-- @attoparsec@.

module System.IO.Streams.Attoparsec
  ( -- * Parsing
    parseFromStream
  , parserToInputStream
  , ParseException(..)
  ) where

import System.IO.Streams.Internal.Attoparsec
    ( ParseException(..), parserToInputStream, parseFromStream )

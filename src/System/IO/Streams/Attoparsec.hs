-- | This module is deprecated -- use
-- System.IO.Streams.Attoparsec.ByteString instead (this module simply
-- re-exports that one).

module System.IO.Streams.Attoparsec
  ( -- * Parsing
    parseFromStream
  , parserToInputStream
  , ParseException(..)
  ) where

------------------------------------------------------------------------------
import           System.IO.Streams.Attoparsec.ByteString (ParseException (..), parseFromStream, parserToInputStream)

-- | This module provides support for parsing values from ByteString
-- 'InputStream's using @attoparsec@.

module System.IO.Streams.Attoparsec.ByteString
  ( -- * Parsing
    parseFromStream
  , parserToInputStream
  , ParseException(..)
  ) where

------------------------------------------------------------------------------
import           Data.Attoparsec.ByteString.Char8      (Parser)
import           Data.ByteString                       (ByteString)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal            (InputStream)
import qualified System.IO.Streams.Internal            as Streams
import           System.IO.Streams.Internal.Attoparsec (ParseData (..), ParseException (..), parseFromStreamInternal)

------------------------------------------------------------------------------
-- | Supplies an @attoparsec@ 'Parser' with an 'InputStream', returning the
-- final parsed value or throwing a 'ParseException' if parsing fails.
--
-- 'parseFromStream' consumes only as much input as necessary to satisfy the
-- 'Parser': any unconsumed input is pushed back onto the 'InputStream'.
--
-- If the 'Parser' exhausts the 'InputStream', the end-of-stream signal is sent
-- to attoparsec.
--
-- Example:
--
-- @
-- ghci> import "Data.Attoparsec.ByteString.Char8"
-- ghci> is <- 'System.IO.Streams.fromList' [\"12345xxx\" :: 'ByteString']
-- ghci> 'parseFromStream' ('Data.Attoparsec.ByteString.Char8.takeWhile' 'Data.Attoparsec.ByteString.Char8.isDigit') is
-- \"12345\"
-- ghci> 'System.IO.Streams.read' is
-- Just \"xxx\"
-- @
parseFromStream :: Parser r
                -> InputStream ByteString
                -> IO r
parseFromStream = parseFromStreamInternal parse feed

------------------------------------------------------------------------------
-- | Given a 'Parser' yielding values of type @'Maybe' r@, transforms an
-- 'InputStream' over byte strings to an 'InputStream' yielding values of type
-- @r@.
--
-- If the parser yields @Just x@, then @x@ will be passed along downstream, and
-- if the parser yields @Nothing@, that will be interpreted as end-of-stream.
--
-- Upon a parse error, 'parserToInputStream' will throw a 'ParseException'.
--
-- Example:
--
-- @
-- ghci> import "Control.Applicative"
-- ghci> import "Data.Attoparsec.ByteString.Char8"
-- ghci> is <- 'System.IO.Streams.fromList' [\"1 2 3 4 5\" :: 'ByteString']
-- ghci> let parser = ('Data.Attoparsec.ByteString.Char8.endOfInput' >> 'Control.Applicative.pure' 'Nothing') \<|\> (Just \<$\> ('Data.Attoparsec.ByteString.Char8.skipWhile' 'Data.Attoparsec.ByteString.Char8.isSpace' *> 'Data.Attoparsec.ByteString.Char8.decimal'))
-- ghci> 'parserToInputStream' parser is >>= 'System.IO.Streams.toList'
-- [1,2,3,4,5]
-- ghci> is' \<- 'System.IO.Streams.fromList' [\"1 2xx3 4 5\" :: 'ByteString'] >>= 'parserToInputStream' parser
-- ghci> 'read' is'
-- Just 1
-- ghci> 'read' is'
-- Just 2
-- ghci> 'read' is'
-- *** Exception: Parse exception: Failed reading: takeWhile1
-- @
parserToInputStream :: Parser (Maybe r)
                    -> InputStream ByteString
                    -> IO (InputStream r)
parserToInputStream = (Streams.makeInputStream .) . parseFromStream
{-# INLINE parserToInputStream #-}

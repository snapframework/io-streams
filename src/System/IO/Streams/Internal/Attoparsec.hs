-- | This module provides support for parsing values from 'InputStream's using
-- @attoparsec@.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module System.IO.Streams.Internal.Attoparsec
  ( -- * Parsing
    ParseException(..)
  , parseFromStream
  , parseFromStreamInternal
  , parserToInputStream
  ) where

------------------------------------------------------------------------------
import           Control.Exception                (Exception, throwIO)
import           Control.Monad                    (when)
import           Data.Attoparsec.ByteString.Char8 (Parser, Result,
                                                   eitherResult, feed, parse)
import           Data.Attoparsec.Types            (IResult (..))
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8 as S       (null)
import           Data.Typeable                    (Typeable)
import           Prelude                          hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal       (InputStream)
import qualified System.IO.Streams.Internal       as Streams

------------------------------------------------------------------------------
-- | An exception raised when parsing fails.
data ParseException = ParseException String
  deriving (Typeable)

instance Show ParseException where
    show (ParseException s) = "Parse exception: " ++ s

instance Exception ParseException

------------------------------------------------------------------------------
-- | Supplies an @attoparsec@ 'Parser' with an 'InputStream', returning the
-- final parsed value or a 'ParseException' if parsing fails.
--
-- 'parseFromStream' consumes only as much input as necessary to satisfy the
-- 'Parser' and unconsumed input is pushed back onto the 'InputStream'.
--
-- If the 'Parser' exhausts the 'InputStream', it receives an @EOF@.
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
{-# INLINE parseFromStream #-}


------------------------------------------------------------------------------
-- | Internal version of parseFromStream allowing dependency injection of the
-- parse functions for testing.
parseFromStreamInternal :: (Parser r -> ByteString -> Result r)
                        -> (Result r -> ByteString -> Result r)
                        -> Parser r
                        -> InputStream ByteString
                        -> IO r
parseFromStreamInternal parseFunc feedFunc parser is =
    Streams.read is >>=
    maybe (finish $ parseFunc parser "")
          (\s -> if S.null s
                   then parseFromStreamInternal parseFunc feedFunc parser is
                   else go $! parseFunc parser s)
  where
    leftover x = when (not $ S.null x) $ Streams.unRead x is

    finish k = let k' = feedFunc (feedFunc k "") ""
               in case k' of
                    Fail x _ _ -> leftover x >> err k'
                    Partial _  -> err k'                -- should be impossible
                    Done x r   -> leftover x >> return r

    err r = let (Left s) = eitherResult r in throwIO $ ParseException s

    go r@(Fail x _ _) = leftover x >> err r
    go (Done x r)     = leftover x >> return r
    go r              = Streams.read is >>=
                        maybe (finish r)
                              (\s -> if S.null s
                                       then go r
                                       else go $! feedFunc r s)


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

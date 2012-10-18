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
import           Control.Exception                ( Exception, throwIO )
import           Control.Monad                    ( when )
import           Data.Attoparsec.ByteString.Char8 ( IResult(..)
                                                  , Parser
                                                  , Result
                                                  , eitherResult
                                                  , feed
                                                  , parse
                                                  )
import           Data.ByteString.Char8            ( ByteString )
import qualified Data.ByteString.Char8            as S
import           Data.Typeable                    ( Typeable )
import           Prelude                   hiding ( read )
------------------------------------------------------------------------------
import           System.IO.Streams.Internal       ( InputStream
                                                  , makeInputStream
                                                  , read
                                                  , unRead
                                                  )

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
parseFromStreamInternal parseFunc feedFunc parser is = do
    read is >>= maybe (finish $ parseFunc parser "")
                      (go . parseFunc parser)
  where
    leftover x = when (not $ S.null x) $ unRead x is

    finish k = let k' = feedFunc (feedFunc k "") ""
               in case k' of
                    Fail x _ _ -> leftover x >> err k'
                    Partial _  -> err k'                -- should be impossible
                    Done x r   -> leftover x >> return r

    err r = let (Left s) = eitherResult r in throwIO $ ParseException s

    go r@(Fail x _ _) = leftover x >> err r
    go (Done x r)     = leftover x >> return r
    go r              = read is >>= maybe (finish r) (go . feedFunc r)


------------------------------------------------------------------------------
-- | Given a 'Parser' yielding values of type @'Maybe' r@, transforms an
-- 'InputStream' over byte strings to an 'InputStream' yielding values of type
-- @r@.
--
-- If the parser yields @Just x@, then @x@ will be passed along downstream, and
-- if the parser yields @Nothing@, that will be interpreted as end-of-stream.
--
-- Upon a parse error, 'parserToInputStream' will throw a 'ParseException'.
parserToInputStream :: Parser (Maybe r)
                    -> InputStream ByteString
                    -> IO (InputStream r)
parserToInputStream = (makeInputStream .) . parseFromStream
{-# INLINE parserToInputStream #-}

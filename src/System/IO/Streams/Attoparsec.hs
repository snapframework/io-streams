-- | This module provides @attoparsec@ integration, converting 'Parser's into
-- 'InputStream's.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module System.IO.Streams.Attoparsec
  ( -- * Parsing
    ParseException(..)
  , parseFromStream
    ) where

------------------------------------------------------------------------------
import           Control.Exception                ( Exception, throwIO )
import           Control.Monad                    ( when )
import           Data.Attoparsec.ByteString.Char8 ( Parser
                                                  , IResult(..)
                                                  , eitherResult
                                                  , feed
                                                  , parse
                                                  )
import           Data.ByteString.Char8            ( ByteString )
import qualified Data.ByteString.Char8            as S
import           Data.Typeable                    ( Typeable )
import           Prelude                   hiding ( read )
------------------------------------------------------------------------------
import           System.IO.Streams.Internal       ( InputStream, read, unRead )

------------------------------------------------------------------------------
-- | An exception raised when parsing fails
data ParseException = ParseException String
  deriving (Typeable)

instance Show ParseException where
    show (ParseException s) = "Parse exception: " ++ s

instance Exception ParseException


------------------------------------------------------------------------------
-- | Supply an @attoparsec@ 'Parser' with an 'InputStream', returning the final
-- parsed value or a 'ParseException' if parsing fails.
--
-- 'parseFromStream' consumes only as much input as necessary to satisfy the
-- 'Parser' and unconsumed input is pushed back onto the 'InputStream.
--
-- If the 'Parser' exhausts the 'InputStream', it receives an @EOF@.
parseFromStream :: Parser r
                -> InputStream ByteString
                -> IO r
parseFromStream parser is = do
    read is >>= maybe (finish $ parse parser "")
                      (go . parse parser)

  where
    leftover x = when (not $ S.null x) $ unRead x is

    finish k = let k' = feed (feed k "") ""
               in case k' of
                    Fail x _ _ -> leftover x >> err k'
                    Partial _  -> err k'
                    Done x r   -> leftover x >> return r


    err r = let (Left s) = eitherResult r in throwIO $ ParseException s

    go r@(Fail x _ _) = leftover x >> err r
    go (Done x r)     = leftover x >> return r
    go r              = read is >>= maybe (finish r) (go . feed r)

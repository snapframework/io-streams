-- | This module provides support for parsing values from 'InputStream's using
-- @attoparsec@.

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}

module System.IO.Streams.Internal.Attoparsec
  ( -- * Parsing
    parseFromStreamInternal

  , ParseData(..)

    -- * Parse Exceptions
  , ParseException(..)

  , eitherResult
  ) where

------------------------------------------------------------------------------
import           Control.Exception                (Exception, throwIO)
import           Control.Monad                    (unless)
import qualified Data.Attoparsec.ByteString.Char8 as S
import qualified Data.Attoparsec.Text             as T
import           Data.Attoparsec.Types            (IResult (..), Parser)
import qualified Data.ByteString                  as S
import           Data.List                        (intercalate)
import           Data.String                      (IsString)
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Prelude                          hiding (null, read)
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
class (IsString i) => ParseData i where
  parse :: Parser i a -> i -> IResult i a
  feed :: IResult i r -> i -> IResult i r
  null :: i -> Bool


------------------------------------------------------------------------------
instance ParseData S.ByteString where
  parse = S.parse
  feed = S.feed
  null = S.null


------------------------------------------------------------------------------
instance ParseData T.Text where
  parse = T.parse
  feed = T.feed
  null = T.null


------------------------------------------------------------------------------
-- | Internal version of parseFromStream allowing dependency injection of the
-- parse functions for testing.
parseFromStreamInternal :: ParseData i
                        => (Parser i r -> i -> IResult i r)
                        -> (IResult i r -> i -> IResult i r)
                        -> Parser i r
                        -> InputStream i
                        -> IO r
parseFromStreamInternal parseFunc feedFunc parser is =
    Streams.read is >>=
    maybe (finish $ parseFunc parser "")
          (\s -> if null s
                   then parseFromStreamInternal parseFunc feedFunc parser is
                   else go $! parseFunc parser s)
  where
    leftover x = unless (null x) $ Streams.unRead x is

    finish k = let k' = feedFunc (feedFunc k "") ""
               in case k' of
                    Fail x _ _ -> leftover x >> err k'
                    Partial _  -> err k'                -- should be impossible
                    Done x r   -> leftover x >> return r

    err r = let (Left (!_,c,m)) = eitherResult r
            in throwIO $ ParseException (ctxMsg c ++ m)

    ctxMsg [] = ""
    ctxMsg xs = "[parsing " ++ intercalate "/" xs ++ "] "

    go r@(Fail x _ _) = leftover x >> err r
    go (Done x r)     = leftover x >> return r
    go r              = Streams.read is >>=
                        maybe (finish r)
                              (\s -> if null s
                                       then go r
                                       else go $! feedFunc r s)


------------------------------------------------------------------------------
-- A replacement for attoparsec's 'eitherResult', which discards information
-- about the context of the failed parse.
eitherResult :: IsString i => IResult i r -> Either (i, [String], String) r
eitherResult (Done _ r)              = Right r
eitherResult (Fail residual ctx msg) = Left (residual, ctx, msg)
eitherResult _                       = Left ("", [], "Result: incomplete input")

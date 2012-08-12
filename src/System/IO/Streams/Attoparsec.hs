{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module System.IO.Streams.Attoparsec
  ( ParseException(..)
  , parseFromStream
  , parseFromStreamWithLeftovers
  )
where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad                    (when)
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as S
import           Data.Maybe
import           Data.Typeable
import           Prelude                          hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal

------------------------------------------------------------------------------
data ParseException = ParseException String
  deriving (Typeable)

instance Show ParseException where
    show (ParseException s) = "Parse exception: " ++ s

instance Exception ParseException


------------------------------------------------------------------------------
parseFromStreamWithLeftovers :: Parser r
                             -> InputStream ByteString
                             -> IO (Either String r, ByteString)
parseFromStreamWithLeftovers parser is = do
    read is >>= maybe (finish $ parse parser "")
                      (go . parse parser)

  where
    finish k = let k' = feed (feed k "") ""
               in case k' of
                    Fail x _ _ -> return (err k' , x )
                    Partial _  -> return (err k' , "")
                    Done x r   -> return (Right r, x )

    err r = eitherResult r

    go r@(Fail x _ _) = return (err r, x)
    go (Done x r)     = return (Right r, x)
    go r              = read is >>= maybe (finish r) (go . feed r)


parseFromStream :: Parser r
                -> InputStream ByteString
                -> IO r
parseFromStream parser is = do
    (e, l) <- parseFromStreamWithLeftovers parser is
    when (not $ S.null l) $ unRead l is
    either (throwIO . ParseException) return e

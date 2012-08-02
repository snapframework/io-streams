{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module System.IO.Streams.Attoparsec where

------------------------------------------------------------------------------
import Control.Exception
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8            (ByteString)
import Data.Maybe
import Data.Typeable
import Prelude                          hiding (read)
------------------------------------------------------------------------------
import System.IO.Streams.Internal

------------------------------------------------------------------------------
data ParseException = ParseException String
  deriving (Typeable)

instance Show ParseException where
    show (ParseException s) = "Parse exception: " ++ s

instance Exception ParseException


------------------------------------------------------------------------------
parseFromStream :: Parser r
                -> InputStream ByteString
                -> IO r
parseFromStream parser is = do
    read is >>= maybe (finish $ parse parser "")
                      (go . parse parser)

  where
    finish k = let k' = feed (feed k "") ""
               in case k' of
                    Fail x _ _ -> unRead x is >> err k'
                    Partial _  -> err k'
                    Done x r   -> unRead x is >> return r


    err r = let (Left s) = eitherResult r in throwIO $ ParseException s

    go r@(Fail x _ _) = unRead x is >> err r
    go (Done x r)     = unRead x is >> return r
    go r              = read is >>= maybe (finish r) (go . feed r)

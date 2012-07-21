{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module System.IO.Streams
 ( InputStream
 , OutputStream
 , read
 , unRead
 , write
 , connect
 , makeInputStream
 , makeOutputStream
 , fromList
 , listOutputStream
 , toList
 ) where

import           Prelude hiding (read)
import           System.IO.Streams.Internal

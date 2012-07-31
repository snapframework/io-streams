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
 , lockingInputStream
 , lockingOutputStream

 , module System.IO.Streams.Blaze
 , module System.IO.Streams.ByteString
 , module System.IO.Streams.Combinators
 , module System.IO.Streams.Handle
 , module System.IO.Streams.List
 , module System.IO.Streams.Zlib
 ) where

------------------------------------------------------------------------------
import           Prelude hiding (read)

------------------------------------------------------------------------------
import           System.IO.Streams.Internal

import           System.IO.Streams.Blaze
import           System.IO.Streams.ByteString
import           System.IO.Streams.Combinators
import           System.IO.Streams.Handle
import           System.IO.Streams.List
import           System.IO.Streams.Zlib

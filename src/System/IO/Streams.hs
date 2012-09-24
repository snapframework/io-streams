{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | It is recommended to import this module qualified, as follows:
--
-- > import           System.IO.Streams (InputStream, OutputStream)
-- > import qualified System.IO.Streams as Streams
--
module System.IO.Streams
 ( InputStream
 , OutputStream
 , nullInput
 , nullOutput
 , read
 , peek
 , unRead
 , write
 , atEOF
 , connect
 , connectTo
 , connectWithoutEof
 , connectToWithoutEof
 , makeInputStream
 , makeOutputStream
 , lockingInputStream
 , lockingOutputStream

 , module System.IO.Streams.Blaze
 , module System.IO.Streams.ByteString
 , module System.IO.Streams.Combinators
 , module System.IO.Streams.Handle
 , module System.IO.Streams.File
 , module System.IO.Streams.List
 , module System.IO.Streams.Zlib
 ) where

------------------------------------------------------------------------------
import           Prelude ()

------------------------------------------------------------------------------
import           System.IO.Streams.Internal

import           System.IO.Streams.Blaze
import           System.IO.Streams.ByteString
import           System.IO.Streams.Combinators
import           System.IO.Streams.File
import           System.IO.Streams.Handle
import           System.IO.Streams.List
import           System.IO.Streams.Zlib

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | It is recommended to import this module qualified, as follows:
--
-- > import           System.IO.Streams (InputStream, OutputStream)
-- > import qualified System.IO.Streams as Streams
--
module System.IO.Streams
 ( -- * Stream types
   InputStream
 , OutputStream

   -- * Build streams
 , makeInputStream
 , makeOutputStream

   -- * Primitive stream operations
 , read
 , unRead
 , peek
 , write
 , atEOF

   -- * Connect streams
 , connect
 , connectTo
 , supply
 , supplyTo

   -- * Thread safety
 , lockingInputStream
 , lockingOutputStream

   -- * Utility streams
 , nullInput
 , nullOutput

   -- * Batteries included
 , module System.IO.Streams.Builder
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

import           System.IO.Streams.Builder
import           System.IO.Streams.ByteString
import           System.IO.Streams.Combinators
import           System.IO.Streams.File
import           System.IO.Streams.Handle
import           System.IO.Streams.List
import           System.IO.Streams.Zlib

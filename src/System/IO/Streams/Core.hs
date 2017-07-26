{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Core types and functions for the @io-streams@ library.
--
module System.IO.Streams.Core
 ( -- * Stream types
   InputStream
 , OutputStream

   -- * Creating streams
 , makeInputStream
 , makeOutputStream

   -- * Primitive stream operations
 , read
 , unRead
 , peek
 , write
 , writeTo
 , atEOF

   -- * Connecting streams together
 , connect
 , connectTo
 , supply
 , supplyTo
 , appendInputStream
 , concatInputStreams

   -- * Thread safety \/ concurrency
 , lockingInputStream
 , lockingOutputStream

   -- * Utility streams
 , nullInput
 , nullOutput

   -- * Generator monad
 , Generator
 , fromGenerator
 , yield
 ) where

------------------------------------------------------------------------------
import           Prelude                    ()
import           System.IO.Streams.Internal

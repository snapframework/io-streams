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
 , atEOF

   -- * Connecting streams together
 , connect
 , connectTo
 , supply
 , supplyTo

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

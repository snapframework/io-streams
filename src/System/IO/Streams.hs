{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | This module is a top-level convenience module which re-exports most of the
-- @io-streams@ library.
--
-- It is recommended to import this module qualified, as follows:
--
-- @
-- import           "System.IO.Streams" ('Generator', 'InputStream', 'OutputStream')
-- import qualified "System.IO.Streams" as Streams
-- @
--
-- For an in-depth tutorial on how to use @io-streams@, please see the
-- "System.IO.Streams.Tutorial" module.
--
module System.IO.Streams
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
    -- $generator
  , Generator
  , fromGenerator
  , yield

   -- * Batteries included
 , module System.IO.Streams.Builder
 , module System.IO.Streams.ByteString
 , module System.IO.Streams.Combinators
 , module System.IO.Streams.Handle
 , module System.IO.Streams.File
 , module System.IO.Streams.List
 , module System.IO.Streams.Vector
 , module System.IO.Streams.Zlib
 ) where

------------------------------------------------------------------------------
import           Prelude                       ()

------------------------------------------------------------------------------
import           System.IO.Streams.Internal

import           System.IO.Streams.Builder
import           System.IO.Streams.ByteString
import           System.IO.Streams.Combinators
import           System.IO.Streams.File
import           System.IO.Streams.Handle
import           System.IO.Streams.List
import           System.IO.Streams.Vector
import           System.IO.Streams.Zlib

------------------------------------------------------------------------------
-- $generator
-- #generator#
--
-- The 'Generator' monad makes it easier for you to define more complicated
-- 'InputStream's. Generators have a couple of basic features:
--
-- 'Generator' is a 'MonadIO', so you can run IO actions from within it using
-- 'liftIO':
--
-- @
-- foo :: 'Generator' r a
-- foo = 'liftIO' fireTheMissiles
-- @
--
-- 'Generator' has a 'yield' function:
--
-- @
-- 'yield' :: r -> 'Generator' r ()
-- @
--
-- A call to \"'yield' @x@\" causes \"'Just' @x@\" to appear when reading the
-- 'InputStream'. Finally, 'Generator' comes with a function to turn a
-- 'Generator' into an 'InputStream':
--
-- @
-- 'fromGenerator' :: 'Generator' r a -> 'IO' ('InputStream' r)
-- @
--
-- Once the 'Generator' action finishes, 'fromGenerator' will cause an
-- end-of-stream 'Nothing' marker to appear at the output. Example:
--
-- @
-- ghci> (Streams.'fromGenerator' $ 'Control.Monad.sequence' $ 'Prelude.map' Streams.'yield' [1..5::Int]) >>= Streams.'toList'
-- [1,2,3,4,5]
-- @

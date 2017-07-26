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
-- Is there a function missing from this library? Interested in contributing?
-- Send a pull request to <http://github.com/snapframework/io-streams>.
module System.IO.Streams
 ( -- * Stream types
   InputStream
 , OutputStream


   -- ** A note about resource acquisition\/release semantics
   -- $resource

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
 , module System.IO.Streams.Network
 , module System.IO.Streams.Process
 , module System.IO.Streams.Text
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
import           System.IO.Streams.Network
import           System.IO.Streams.Process
import           System.IO.Streams.Text
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


------------------------------------------------------------------------------
-- $resource
-- #resource#
--
-- In general, the convention within this library is that input and output
-- streams do not deal with resource acquisition\/release semantics, with rare
-- exceptions like 'System.IO.Streams.withFileAsInput'. For example, sending
-- \"end-of-stream\" to an 'OutputStream' wrapped around a 'System.IO.Handle'
-- doesn't cause the handle to be closed. You can think of streams as little
-- state machines that are attached to the underlying resources, and the
-- finalization\/release of these resources is up to you.
--
-- This means that you can use standard Haskell idioms like
-- 'Control.Exception.bracket' to handle resource acquisition and cleanup in an
-- exception-safe way.
--


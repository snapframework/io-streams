# Version 1.4.1.0

- Added `writeTo` export to the main module (forgotten when it was added to
  `.Core`.)

# Version 1.4.0.0

- Added support for Text with Attoparsec, courtesy Kevin Brubeck Unhammer. Adds
  modules `System.IO.Streams.Attoparsec.{ByteString, Text}` and deprecates
  `System.IO.Streams.Attoparsec`, which is now a thin wrapper.

# Version 1.3.6.1
- Bumped dependencies on `time` and `process`.

# Version 1.3.6.0
  - Added new fold functions:
  ```haskell
fold_ :: (x -> a -> x)    -- ^ accumulator update function
      -> x                -- ^ initial seed
      -> (x -> s)         -- ^ recover folded value
      -> InputStream a    -- ^ input stream
      -> IO s
foldM_ :: (x -> a -> IO x)   -- ^ accumulator update action
       -> IO x               -- ^ initial seed
       -> (x -> IO s)        -- ^ recover folded value
       -> InputStream a      -- ^ input stream
       -> IO s
  ```

# Version 1.3.5.0
  - Add support for latest `process`, `time`, and `transformers` releases
    (and thereby indirectly for the upcoming GHC 8.0).

# Version 1.3.4.0
  - Added `System.IO.Streams.Handle.handleToStreams`, to conveniently
    create an `InputStream`/`OutputStream` pair.

# Version 1.3.3.1
  - Fixed a testsuite compile error on GHC >= 7.10.

# Version 1.3.3.0
  - Added a new convenience function, like `chunkList` but with a predicate for
    when to split, taking current element and current chunk length:
    ```haskell
chunkListWith :: (a -> Int -> Bool) -> InputStream a -> IO (InputStream [a])
    ```

# Version 1.3.2.0
  - Dependency bump for attoparsec 0.13 (another location)
  - Dependency bump for vector 0.11
  - Dependency bump for zlib 0.6

# Version 1.3.1.0
  - Dependency bump for attoparsec 0.13.

# Version 1.3.0.0
  - As long promised, removed the direct use of the `blaze-builder` package in
    favor of the new `bytestring-builder` transitional package (to be replaced
    by bytestring's native builder once it is mature enough).
  - Added a new convenience function, a flipped version of `write`:
    ```haskell
writeTo :: OutputStream a -> Maybe a -> IO ()
    ```

# Version 1.2.1.3
  - Dependency bump for primitive 0.6.

# Version 1.2.1.2
  - Dependency bump for deepseq 1.4.

# Version 1.2.1.1
  - Dependency bump for time 1.6.

# Version 1.2.1.0
  - Added `System.IO.Streams.mapMaybe` for InputStream.

  - Added `System.IO.Streams.contramapMaybe` for OutputStream.

# Version 1.2.0.1

  - `System.IO.Streams.Attoparsec.parseFromStream`: export more information
    about the context of parse errors to the message returned via
    `ParseException`.

  - Improved documentation about stream flushing in the docstring for
    `handleToOutputStream`.

# Version 1.2.0.0
  - Fixed bug #27 (https://github.com/snapframework/io-streams/issues/27):
    makeOutputStream now properly shuts down the stream upon receiving EOF. The
    new invariant might break user programs if they depended on the buggy
    behaviour, which is the reason for the major version bump.

  - Fixed a few polymorphic bindings that started breaking in recent GHC.

  - Dependency bumps for:
    - text 1.2
    - network 2.6

# Version 1.1.4.6
Moved old changelog entries to `changelog.md`.

# Version 1.1.4.5
Allow use of attoparsec 0.12.*.

# Version 1.1.4.4
Allow use of transformers 0.4.*.

# Version 1.1.4.3
Allow use of new network version 2.5.

# Version 1.1.4.2
Fixed a build error with network versions older than 2.4.

# Version 1.1.4.1
`System.IO.Streams.Network`: scalability improvement: buffers for socket reads
are now allocated by system malloc rather than by pinned pointers in GHC
(currently pinned pointer allocation takes a global lock).

# Version 1.1.4.0
Widened `attoparsec` and `text` library dependencies to allow the latest
versions.

# Version 1.1.3.0
Added `System.IO.Streams.ByteString.takeExactly`. Widened `network` dependency
to include 2.3. Added a `NoInteractiveTests` flag to selectively disable some
tests for environments where spawning interactive processes is impossible.

# Version 1.1.2.2
Allowed newest versions of the `process`, `test-framework`, and `text`
libraries.

# Version 1.1.2.1
Fixed build error when compiled against attoparsec-0.10.0.x.

# Version 1.1.2.0
Added `System.IO.Streams.Concurrent.makeChanPipe`, to create a simple
concurrent pipe between an `InputStream`/`OutputStream` pair.

# Version 1.1.1.0
Added `System.IO.Streams.Network.socketToStreamsWithBufferSize`, allowing
control over the size of the receive buffers used when reading from sockets.

# Version 1.1.0.3
Fixed an inconsistent version upper bound in the test suite.

# Version 1.1.0.2
Fixed a typo in the tutorial.

# Version 1.1.0.1
A couple of Haddock markup fixes.

# Version 1.1.0.0
Reworked, simplified, and streamlined the internals of the library. Exports
from `System.IO.Streams.Internal` relying on Sources and Sinks were deleted
because they are no longer necessary: `Source(..)`, `Sink(..)`,
`defaultPushback`, `withDefaultPushback`, `nullSource`, `nullSink`,
`singletonSource`, `simpleSource`, `sourceToStream`, `sinkToStream`,
`generatorToSource`, and `consumerToSink`.

# Version 1.0.2.2
Fixed a bug in which `"takeBytes 0"` was erroneously requesting input from the
wrapped stream.

# Version 1.0.2.1
Fixed a compile error on GHC 7.0.x.

# Version 1.0.2.0
Added `System.IO.Streams.Process` (support for communicating with system
processes using streams), added new functions to `System.IO.Streams.Handle` for
converting `io-streams` types to `System.IO.Handle`s. (Now you can pass streams
from this library to places that expect Handles and everything will work.)

# Version 1.0.1.0
Added `System.IO.Streams.Combinators.ignoreEof`.

# Version 1.0.0.1
Fixed some haddock markup.

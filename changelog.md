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

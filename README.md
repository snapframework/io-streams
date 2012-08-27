The io-streams library contains simple and easy to use primitives for I/O
using streams. Based on simple types with one type parameter (`InputStream a`
and `OutputStream a`), io-streams provides a basic interface to
side-effecting input and output in `IO` monad with the following
features:

  * three fundamental I/O primitives that anyone can understand: `read ::
    InputStream a -> IO (Maybe a)`, `unRead :: a -> InputStream a -> IO ()`,
    and `write :: Maybe a -> OutputStream a -> IO ()`.

  * simple types and side-effecting IO operations mean straightforward and
    simple exception handling and resource cleanup using standard Haskell
    facilities like `bracket`.

  * code to transform files, handles, and sockets to streams

  * a variety of combinators for wrapping and transforming streams, including
    compression and decompression using zlib, controlling precisely how many
    bytes are read to or written from a socket, buffering output using
    `blaze-builder`, etc.

  * support for parsing from streams using `attoparsec`.

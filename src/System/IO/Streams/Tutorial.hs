module System.IO.Streams.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Building IO Streams
    -- $create

    -- * Using IO Streams
    -- $use

    -- * Resource and Exception Safety
    -- $safety

    -- * Caveats
    -- $gotchas
    ) where

import System.IO.Streams
import System.IO (Handle)

{-  NOTE: Please stick to the {- -} comment style until the tutorial stabilizes
    to make it easier for me to reformat the text. -}

{- $introduction

    The @io-streams@ package defines two \"smart handles\" for stream
    processing:

    * 'InputStream': a read-only smart handle

    * 'OutputStream': a write-only smart handle

    The 'InputStream' type implements all the core operations we expect for a
    read-only handle.  We consume values using 'read', which returns a 'Nothing'
    when the resource is done:
    {- NOTE: I'm not sure if I should use the term EOF since it might confuse
             the reader to think these are only for files -}

> read :: InputStream c -> IO (Maybe c)

    ... and we can push back values using 'unRead':

> unRead :: c -> InputStream c -> IO ()

    The 'OutputStream' type implements the 'write' operation which feeds it
    output, supplying 'Nothing' to signal resource exhaustion:

> write :: Maybe c -> OutputStream c -> IO ()

    These streams slightly resemble Haskell 'Handle's, but support a wider
    range of sources and sinks.  For example, you can convert an ordinary list
    to an 'InputStream' source and interact with it using the handle-based API:

>>> import qualified System.IO.Streams as S
>>> listHandle <- S.fromList [1, 2]
>>> read listHandle
Just 1
>>> read listHandle
Just 2
>>> read listHandle
Nothing

    Additionally, IO streams come with a library of stream transformations that
    preserve their handle-like API.  For example, you can map a function over an
    'InputStream', which generates a new handle to the same stream that returns
    transformed values:

>>> oldHandle <- S.fromList [1, 2, 3]
>>> newHandle <- S.mapM (* 10) oldHandle
>>> read newHandle
10
>>> -- We can still view the stream through the old handle
>>> read oldHandle
2
>>> -- ... and switch back again
>>> read newHandle
30

    IO streams focus on preserving the convention of traditional handles while
    offering a wider library of stream-processing utilities.
-}

{- $create
    
    The @io-streams@ library provides a simple interface for creating your
    own 'InputStream's and 'OutputStream's.

    You can build an 'InputStream' from any 'IO' action that generates output,
    as long as it wraps results in 'Just' and uses 'Nothing' to signal EOF:

> makeInputStream :: IO (Maybe a) -> IO (InputStream a)

    Similarly, you can build any 'OutputStream' from an 'IO' action that accepts
    input, as long as it interprets 'Just' as more input and 'Nothing' as EOF:

> makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)

    As an example, let's wrap an ordinary read-only 'Handle' in an
    'InputStream':

> import Data.ByteString (ByteString)
> import qualified Data.ByteString as S
> import System.IO.Streams (InputStream)
> import qualified System.IO.Streams as Streams
> import System.IO (Handle, hFlush)
>
> bUFSIZ = 32752
>
> upgradeReadOnlyHandle :: Handle -> IO (InputStream ByteString)
> upgradeReadOnlyHandle h = Streams.makeInputStream f
>   where
>     f = do
>         x <- S.hGetSome h BUFSIZ
>         return $! if S.null x then Nothing else Just x

    Now that we've upgraded it, we can tap into all the stream-processing
    features this library provides.  For example, we can decompress the input:

> unzipHandle :: Handle -> IO (InputStream ByteString)
> unzipHandle = upgradeReadOnlyHandle >=> Streams.decompress

    ... or we could prevent a denial-of-service attack:

> protectHandle :: Handle -> IO (InputStream ByteString)
> protectHandle =
>     upgradeReadOnlyHandle >=> Streams.throwIfProducesMoreThan 1000000

    We don't even really need to write the @upgradeReadOnlyHandle@ function,
    because this library already provides one that uses the same implementation
    given above:

> -- from System.IO.Streams.Handle
> handleToInputStream :: Handle -> IO (InputStream ByteString)

    @io-streams@ provides many useful functions such as these in its standard
    library and you can take advantage of them by transforming your resources
    into IO streams.
-}

{- $use
-}

{- $safety
    IO streams reuse existing Haskell idioms for resource safety.  Since all
    operations occur in the IO monad, you can use 'catch' or 'bracket' to guard
    any 'read' or 'write' without any special considerations:

> withFile ReadMode $ \handle -> do
>     stream <- handleToInputStream handle
>     mBytes <- read stream
>     case mBytes of
>         Just bytes -> print bytes
>         Nothing    -> putStrLn "EOF"

    However, @io-streams@ also provides @with...@ functions of its own that
    expose 'InputStream's and 'OutputStream's instead of 'Handle's:

> withFileAsInput
>  :: FilePath -> (InputStream ByteString -> IO a) -> IO a
>
> withFileAsOutput
>  :: FilePath -> IOMode -> (OutputStream ByteString -> IO a) -> IO a

-}

{- $gotchas
    {- NOTE: Talk about
        * how multiple streams can point to the same handle
        * how pushback works in the presence of multiple layers
        * thread safety and locking streams
    -}
-}

{- TODO:
   Type-check examples
-}

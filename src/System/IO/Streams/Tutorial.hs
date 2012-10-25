module System.IO.Streams.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Creating IO Streams
    -- $create
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
    read-only handle.  We can consume values using 'read':

    {- NOTE: This doesn't explain why these use 'Maybe'.  I don't know how to
       work that in here without disrupting the flow.  Perhaps put it off to
       a later section. -}
> read :: InputStream c -> IO (Maybe c)

    ... push back values using 'unRead':

> unRead :: c -> InputStream c -> IO ()

    {- NOTE: Maybe skip mentioning 'peek' to emphasize simplicity -}
    ... or 'peek' at the next available value without consuming it:

> peek :: InputStream c -> IO (Maybe c)

    The 'OutputStream' type implements the 'write' operation which feeds it
    output:

> write :: Maybe c -> OutputStream c -> IO ()

    You could use these types as drop-in replacements for any handle-based API,
    but they differ from ordinary handles by supporting a wide variety
    of stream transformations that don't perturb the user-facing API.
    Consequently, users get the best of both worlds: the simplicity and
    convention of handles with the power and flexibility of stream processing.
-}

{- $create
    
    The @io-streams@ library provides a simple interface for creating your
    own 'InputStream's and 'OutputStream's.

    You can build any 'InputStream' from an 'IO' action that generates output,
    as long as it wraps results in 'Just' and uses 'Nothing' to signal EOF:

> makeInputStream :: IO (Maybe a) -> IO (InputStream a)

    Similarly, you can build any 'OutputStream' from an 'IO' action that accepts
    input, as long as it accepts 'Just' for more input and 'Nothing' for EOF:

> makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)

    As an example, let's wrap an ordinary read-only 'Handle' in an
    'InputStream':

    {- NOTE: Perhaps I should match the definition in System.IO.Streams.Handle
             since it's pretty simple already and it would impress the reader
             to later discover that the library's own implementation was in
             fact that simple. -}

> import Data.ByteString (ByteString, hGet)
> import System.IO.Streams (InputStream, OutputStream)
> import qualified System.IO.Streams as Streams
> import System.IO (Handle, hIsEOF)
>
> upgradeReadOnlyHandle :: Handle -> IO (InputStream ByteString)
> upgradeReadOnlyHandle h = Streams.makeInputStream $ do
>     eof <- hIsEOF h
>     if eof
>     then return Nothing
>     else do
>         bytes <- hGet h 4096
>         return (Just bytes)

    Now that we've upgraded it, we can tap into all the stream-processing
    features this library provides.  For example, we can decompress the input:

    {- NOTE: Let me know if you prefer ordinary do notation instead of (>=>) -}

> import System.IO.Streams.Zlib
>
> unzipHandle :: Handle -> IO (InputStream ByteString)
> unzipHandle = upgradeReadOnlyHandle >=> decompress

    ... or we could prevent a denial-of-service attack:

> import System.IO.Streams.ByteString
>
> protectHandle :: Handle -> IO (InputStream ByteString)
> protectHandle = upgradeReadOnlyHandle >=> throwIfProducesMoreThan 1000000

    We don't even really need to write the @upgradeReadOnlyHandle@ function,
    because this library already provides one:

> -- from System.IO.Streams.Handle
> handleToInputStream :: Handle -> IO (InputStream ByteString)

    We see a pattern: @io-streams@ includes a complete library of standard
    functions that anticipates our needs.  You automatically tap into this
    suite of tools whenever you upgrade your resources to IO streams.
-}

{- TODO:
   Type-check examples
-}

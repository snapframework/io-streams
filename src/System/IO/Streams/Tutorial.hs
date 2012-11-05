module System.IO.Streams.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Build Input Streams
    -- $createinput

    -- * Build Output Streams
    -- $createoutput

    -- * Connect Streams
    -- $connect

    -- * Transform Streams
    -- $transform

    -- * Resource and Exception Safety
    -- $safety

    -- * Pushback
    -- $pushback

    -- * Thread Safety
    -- $threadsafety
    ) where

import System.IO.Streams
import System.IO (Handle)

{-  NOTE: Please stick to the {- -} comment style until the tutorial stabilizes
    to make it easier for me to reformat the text. -}

{-  NOTE: How liberal should I be with module imports for each example?  Should
          I respecify the imports with each new section or always assume old
          imports and only specify new ones as I proceed through the tutorial?
-}

{- $introduction

    The @io-streams@ package defines two \"smart handles\" for stream
    processing:

    * 'InputStream': a read-only smart handle

    * 'OutputStream': a write-only smart handle

    The 'InputStream' type implements all the core operations we expect for a
    read-only handle.  We consume values using 'read', which returns a 'Nothing'
    when the resource is done:

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
>>> S.read listHandle
Just 1
>>> S.read listHandle
Just 2
>>> S.read listHandle
Nothing

    Additionally, IO streams come with a library of stream transformations that
    preserve their handle-like API.  For example, you can map a function over an
    'InputStream', which generates a new handle to the same stream that returns
    transformed values:

>>> oldHandle <- S.fromList [1, 2, 3]
>>> newHandle <- S.mapM (\x -> return (x * 10)) oldHandle
>>> S.read newHandle
10
>>> -- We can still view the stream through the old handle
>>> S.read oldHandle
2
>>> -- ... and switch back again
>>> S.read newHandle
30

    IO streams focus on preserving the convention of traditional handles while
    offering a wider library of stream-processing utilities.
-}

{- $createinput
    The @io-streams@ library provides a simple interface for creating your
    own 'InputStream's and 'OutputStream's.

    You can build an 'InputStream' from any 'IO' action that generates output,
    as long as it wraps results in 'Just' and uses 'Nothing' to signal EOF:

> makeInputStream :: IO (Maybe a) -> IO (InputStream a)

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
>         x <- S.hGetSome h bUFSIZ
>         return $! if S.null x then Nothing else Just x

    We didn't even really need to write the @upgradeReadOnlyHandle@ function,
    because "System.IO.Streams.Handle" already provides one that uses the exact
    same implementation given above:

> handleToInputStream :: Handle -> IO (InputStream ByteString)
-}

{- $createoutput
    Similarly, you can build any 'OutputStream' from an 'IO' action that accepts
    input, as long as it interprets 'Just' as more input and 'Nothing' as EOF:

> makeOutputStream :: (Maybe a -> IO ()) -> IO (OutputStream a)

    A simple 'OutputStream' might wrap 'putStrLn' for 'ByteString's:

> import Data.ByteString (ByteString)
> import qualified Data.ByteString as S
> import System.IO.Streams (OutputStream)
> import qualified System.IO.Streams as Streams
>
> writeConsole :: IO (OutputStream ByteString)
> writeConsole = Streams.makeOutputStream $ \m -> case m of
>     Just bs -> S.putStrLn bs
>     Nothing -> return ()

    The 'Just' wraps more incoming data, whereas 'Nothing' indicates the data
    is exhausted.  In principle, you can feed 'OutputStream's more input
    after writing a 'Nothing' to them, but IO streams only guarantee a
    well-defined behavior up to the first 'Nothing'.  After receiving the
    first 'Nothing', an 'OutputStream' could respond to additional input by:

    * Using the input

    * Ignoring the input

    * Throwing an exception

    Ideally, you should adhere to well-defined behavior and ensure that after
    you write a 'Nothing' to an 'OutputStream', you don't write anything else.
-}

{- $connect
    @io-streams@ provides two ways to connect an 'InputStream' and
    'OutputStream' :

> connect :: InputStream a -> OutputStream a -> IO ()
> supply  :: InputStream a -> OutputStream a -> IO ()

    'connect' feeds the 'OutputStream' exclusively with the given
    'InputStream' and passes along the end-of-stream notification to the
    'OutputStream'.

    'supply' feeds the 'OutputStream' non-exclusively with the given
    'InputStream', but does not pass along the end-of-stream notification to the
    'OutputStream'.

    You can combine both 'supply' and 'connect' to feed multiple 'InputStreams'
    into a single 'OutputStream':

> import qualified System.IO.Streams as Streams
> import System.IO (IOMode(WriteMode))
>
> main =
>    Streams.withFileAsOutput "out.txt" WriteMode $ \outStream ->
>    Streams.withFileAsInput  "in1.txt" $ \inStream1 ->
>    Streams.withFileAsInput  "in2.txt" $ \inStream2 ->
>    Streams.withFileAsInput  "in3.txt" $ \inStream3 ->
>    Streams.supply  inStream1 outStream
>    Streams.supply  inStream2 outStream
>    Streams.connect inStream2 outStream

{- NOTE: Maybe shorten the "Streams" qualifier for this example because I want
         the 'connect' vs 'supply' distinction to visually stand out on the last
         three lines. -}

    The final 'connect' seals the 'OutputStream' when the final 'InputStream'
    terminates.

    Keep in mind that you do not need to use 'connect' or 'supply' at all and
    @io-streams@ mainly provides them for user convenience.  You can always
    build your own abstractions on top of the 'read' and 'write' operations.
-}

{- $transform
    When we build or use IO streams we can tap into all the stream-processing
    features the @io-streams@ library provides.  For example, we can decompress
    any 'InputStream' of 'ByteString's:

> import Control.Monad ((>=>))
> import Data.ByteString (ByteString)
> import System.IO (Handle)
> import System.IO.Streams (InputStream, OutputStream)
> import qualified System.IO.Streams as Streams
> import qualified System.IO.Streams.File as Streams
> 
> unzipHandle :: Handle -> IO (InputStream ByteString)
> unzipHandle = Streams.handleToInputStream >=> Streams.decompress

    ... or we can guard it against a denial-of-service attack:

> protectHandle :: Handle -> IO (InputStream ByteString)
> protectHandle =
>     Streams.handleToInputStream >=> Streams.throwIfProducesMoreThan 1000000

    @io-streams@ provides many useful functions such as these in its standard
    library and you take advantage of them by defining IO streams that wrap
    your resources.
-}

{- $safety
    IO streams use standard Haskell idioms for resource safety.  Since all
    operations occur in the IO monad, you can use 'catch', 'bracket', or
    'with...' functions to guard any 'read' or 'write' without any special
    considerations:

> import qualified Data.ByteString as S
> import System.IO
> import System.IO.Streams (InputStream, OutputStream)
> import qualified System.IO.Streams as Streams
> import qualified System.IO.Streams.File as Streams
> 
> main =
>     withFile "test.txt" ReadMode $ \handle -> do
>         stream <- Streams.handleToInputStream handle
>         mBytes <- Streams.read stream
>         case mBytes of
>             Just bytes -> S.putStrLn bytes
>             Nothing    -> putStrLn "EOF

    However, you can also simplify the above example by using the convenience
    function 'withFileAsInput' from "System.IO.Streams.File":

> withFileAsInput
>  :: FilePath -> (InputStream ByteString -> IO a) -> IO a
>
> withFile "test.txt" ReadMode $ \handle -> do
>     stream <- Streams.handleToInputStream handle
>     mBytes <- Streams.read stream
>     case mBytes of
>         Just bytes -> S.putStrLn bytes
>         Nothing    -> putStrLn "EOF"


-}

{- $pushback
    All 'InputStream's support pushback, which simplifies many types of
    operations.  For example, we can 'peek' at an 'InputStream' by combining
    'read' and 'unRead':

> peek :: InputStream c -> IO (Maybe c)
> peek s = do
>     x <- Streams.read s
>     case x of
>         Nothing -> return ()
>         Just c  -> Streams.unRead c s
>     return x

    ... although "System.IO.Streams" already exports the above function.

    'InputStream's can customize pushback behavior to support more sophisticated
    support for pushback.  For example, if you protect a stream using
    'throwIfProducesMoreThan' and 'unRead' input, it will subtract the unread
    input from the total byte count.  However, these extra features will not
    interfer with the basic pushback contract, given by the following law:

> unRead c stream >> read stream == return (Just c)

    When you build an 'InputStream' using 'makeInputStream', it supplies the
    default pushback behavior which just saves the input for the next 'read'
    call.  More advanced users can use "System.IO.Streams.Internal" to
    customize their own pushback routines.
{- NOTE: The library only exports pushback API for Sources, which are a
         completely internal type, so should we teach the user how to define
         custom pushback or not?  Maybe that belongs in some sort of separate
         "advanced" tutorial for System.IO.Streams.Internal. -}
-}

{- $threadsafety
    IO stream operations are not thread-safe by default for performance reasons.
    However, you can transform an existing IO stream into a thread-safe one
    using the provided locking functions:

> lockingInputStream  :: InputStream  a -> IO (InputStream  a)
> lockingOutputStream :: OutputStream a -> IO (OutputStream a)
>
> threadSafeOutputStream :: Handle -> IO (OutputStream a)
> threadSafeOutputStream =
>     Streams.handleToOutputStream >=> Streams.lockingOutputStream

    These functions do not prevent access to the previous IO stream, so you must
    take care to not save the reference to the previous stream.

    {- NOTE: Should I give specific performance numbers or just say something
             like "a slight cost to performance" for locking? -}
    {- NOTE: This could use a concrete example of a race condition that a user
             might encounter without this protection. -}
-}

{- NOTE: Should there be some sort of concluding section? -}

{- TODO:
   Type-check examples
-}

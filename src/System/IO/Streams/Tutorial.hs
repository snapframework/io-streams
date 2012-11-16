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

{-  NOTE: Please stick to the {- -} comment style until the tutorial stabilizes
    to make it easier for me to reformat the text. -}

{-  NOTE: How liberal should I be with module imports for each example?  Should
          I respecify the imports with each new section or always assume old
          imports and only specify new ones as I proceed through the tutorial?
-}

{- $introduction

The @io-streams@ package defines two \"smart handles\" for stream processing:

  * 'System.IO.Streams.InputStream': a read-only smart handle

  * 'System.IO.Streams.OutputStream': a write-only smart handle

The 'System.IO.Streams.InputStream' type implements all the core operations we
expect for a read-only handle. We consume values using 'read', which returns a
'Nothing' when the resource is done:

@
'System.IO.Streams.read' :: 'System.IO.Streams.InputStream' c -> 'IO' ('Maybe' c)
@

... and we can push back values using 'System.IO.Streams.unRead':

@
'System.IO.Streams.unRead' :: c -> 'System.IO.Streams.InputStream' c -> 'IO' ()
@

The 'System.IO.Streams.OutputStream' type implements the
'System.IO.Streams.write' operation which feeds it output, supplying 'Nothing'
to signal resource exhaustion:

@
'System.IO.Streams.write' :: 'Maybe' c -> 'System.IO.Streams.OutputStream' c -> 'IO' ()
@

These streams slightly resemble Haskell 'System.IO.Handle's, but support a
wider range of sources and sinks. For example, you can convert an ordinary list
to an 'System.IO.Streams.InputStream' source and interact with it using the
handle-based API:

@
ghci> import qualified System.IO.Streams as S
ghci> listHandle \<- S.'System.IO.Streams.fromList' [1, 2]
ghci> S.'System.IO.Streams.read' listHandle
Just 1
ghci> S.'System.IO.Streams.read' listHandle
Just 2
ghci> S.'System.IO.Streams.read' listHandle
Nothing
@

Additionally, IO streams come with a library of stream transformations that
preserve their handle-like API. For example, you can map a function over an
'System.IO.Streams.InputStream', which generates a new handle to the same
stream that returns transformed values:

@
ghci> oldHandle <- S.'System.IO.Streams.fromList' [1, 2, 3]
ghci> newHandle <- S.'System.IO.Streams.mapM' (\x -> 'return' (x * 10)) oldHandle
ghci> S.'System.IO.Streams.read' newHandle
10
ghci> -- We can still view the stream through the old handle
ghci> S.'System.IO.Streams.read' oldHandle
2
ghci> -- ... and switch back again
ghci> S.'System.IO.Streams.read' newHandle
30
@

IO streams focus on preserving the convention of traditional handles while
offering a wider library of stream-processing utilities.

-}

{- $createinput


The @io-streams@ library provides a simple interface for creating your own
'System.IO.Streams.InputStream's and 'System.IO.Streams.OutputStream's.

You can build an 'System.IO.Streams.InputStream' from any 'IO' action that
generates output, as long as it wraps results in 'Just' and uses 'Nothing' to
signal EOF:

@
'System.IO.Streams.makeInputStream' :: 'IO' ('Maybe' a) -> 'IO' ('System.IO.Streams.InputStream' a)
@

As an example, let's wrap an ordinary read-only 'System.IO.Handle' in an
'System.IO.Streams.InputStream':

@
import "Data.ByteString" ('Data.ByteString.ByteString')
import qualified "Data.ByteString" as S
import "System.IO.Streams" ('System.IO.Streams.InputStream')
import qualified "System.IO.Streams" as Streams
import "System.IO" ('System.IO.Handle', 'System.IO.hFlush')

bUFSIZ = 32752

upgradeReadOnlyHandle :: 'System.IO.Handle' -> 'IO' ('System.IO.Streams.InputStream' 'Data.ByteString.ByteString')
upgradeReadOnlyHandle h = Streams.'System.IO.Streams.makeInputStream' f
  where
    f = do
        x <- S.'Data.ByteString.hGetSome' h bUFSIZ
        'return' $! if S.'Data.ByteString.null' x then 'Nothing' else 'Just' x
@

We didn't even really need to write the @upgradeReadOnlyHandle@ function,
because "System.IO.Streams.Handle" already provides one that uses the exact
same implementation given above:

@
'System.IO.Streams.handleToInputStream' :: 'System.IO.Handle' -> 'IO' ('System.IO.Streams.InputStream' 'Data.ByteString.ByteString')
@

-}

{- $createoutput

Similarly, you can build any 'System.IO.Streams.OutputStream' from an 'IO'
action that accepts input, as long as it interprets 'Just' as more input and
'Nothing' as EOF:

@
'System.IO.Streams.makeOutputStream' :: ('Maybe' a -> 'IO' ()) -> 'IO' ('System.IO.Streams.OutputStream' a)
@

A simple 'System.IO.Streams.OutputStream' might wrap 'putStrLn' for 'Data.ByteString.ByteString's:

@
import "Data.ByteString" ('Data.ByteString.ByteString')
import qualified "Data.ByteString" as S
import "System.IO.Streams" ('System.IO.Streams.OutputStream')
import qualified "System.IO.Streams" as Streams
\
writeConsole :: 'IO' ('System.IO.Streams.OutputStream' 'Data.ByteString.ByteString')
writeConsole = Streams.'System.IO.Streams.makeOutputStream' $ \m -> case m of
    'Just' bs -> S.'Data.ByteString.putStrLn' bs
    'Nothing' -> 'return' ()
@

The 'Just' wraps more incoming data, whereas 'Nothing' indicates the data is
exhausted. In principle, you can feed 'System.IO.Streams.OutputStream's more
input after writing a 'Nothing' to them, but IO streams only guarantee a
well-defined behavior up to the first 'Nothing'. After receiving the first
'Nothing', an 'System.IO.Streams.OutputStream' could respond to additional
input by:

  * Using the input

  * Ignoring the input

  * Throwing an exception

Ideally, you should adhere to well-defined behavior and ensure that after you
write a 'Nothing' to an 'System.IO.Streams.OutputStream', you don't write
anything else.

-}

{- $connect

@io-streams@ provides two ways to connect an 'System.IO.Streams.InputStream'
and 'System.IO.Streams.OutputStream':

@
'System.IO.Streams.connect' :: 'System.IO.Streams.InputStream' a -> 'System.IO.Streams.OutputStream' a -> 'IO' ()
'System.IO.Streams.supply'  :: 'System.IO.Streams.InputStream' a -> 'System.IO.Streams.OutputStream' a -> 'IO' ()
@

'System.IO.Streams.connect' feeds the 'System.IO.Streams.OutputStream'
exclusively with the given 'System.IO.Streams.InputStream' and passes along the
end-of-stream notification to the 'System.IO.Streams.OutputStream'.

'System.IO.Streams.supply' feeds the 'System.IO.Streams.OutputStream'
non-exclusively with the given 'System.IO.Streams.InputStream', but does not
pass along the end-of-stream notification to the
'System.IO.Streams.OutputStream'.

You can combine both 'System.IO.Streams.supply' and 'System.IO.Streams.connect'
to feed multiple 'System.IO.Streams.InputStream's into a single
'System.IO.Streams.OutputStream':

@
import qualified "System.IO.Streams" as Streams
import "System.IO" ('System.IO.IOMode'('System.IO.WriteMode'))

main = do
   Streams.'System.IO.Streams.withFileAsOutput' \"out.txt\" 'System.IO.WriteMode' $ \outStream ->
   Streams.'System.IO.Streams.withFileAsInput'  \"in1.txt\" $ \inStream1 ->
   Streams.'System.IO.Streams.withFileAsInput'  \"in2.txt\" $ \inStream2 ->
   Streams.'System.IO.Streams.withFileAsInput'  \"in3.txt\" $ \inStream3 ->
   Streams.'System.IO.Streams.supply'  inStream1 outStream
   Streams.'System.IO.Streams.supply'  inStream2 outStream
   Streams.'System.IO.Streams.connect' inStream2 outStream
@

{- NOTE: Maybe shorten the "Streams" qualifier for this example because I want
         the 'connect' vs 'supply' distinction to visually stand out on the last
         three lines. -}

The final 'System.IO.Streams.connect' seals the
'System.IO.Streams.OutputStream' when the final 'System.IO.Streams.InputStream'
terminates.

Keep in mind that you do not need to use 'System.IO.Streams.connect' or
'System.IO.Streams.supply' at all: @io-streams@ mainly provides them for user
convenience. You can always build your own abstractions on top of the
'System.IO.Streams.read' and 'System.IO.Streams.write' operations.

-}

{- $transform

When we build or use 'IO' streams we can tap into all the stream-processing
features the @io-streams@ library provides. For example, we can decompress any
'System.IO.Streams.InputStream' of 'Data.ByteString.ByteString's:

@
import "Control.Monad" ((>=>))
import "Data.ByteString" ('Data.ByteString.ByteString')
import "System.IO" ('System.IO.Handle')
import "System.IO.Streams" ('System.IO.Streams.InputStream', 'System.IO.Streams.OutputStream')
import qualified "System.IO.Streams" as Streams
import qualified "System.IO.Streams.File" as Streams

unzipHandle :: 'System.IO.Handle' -> 'IO' ('System.IO.Streams.InputStream' 'Data.ByteString.ByteString')
unzipHandle = Streams.'System.IO.Streams.handleToInputStream' >=> Streams.'System.IO.Streams.decompress'
@

... or we can guard it against a denial-of-service attack:

@
protectHandle :: 'System.IO.Handle' -> 'IO' ('System.IO.Streams.InputStream' 'Data.ByteString.ByteString')
protectHandle =
    Streams.'System.IO.Streams.handleToInputStream' >=> Streams.'System.IO.Streams.throwIfProducesMoreThan' 1000000
@

@io-streams@ provides many useful functions such as these in its standard
library and you take advantage of them by defining IO streams that wrap your
resources.

-}

{- $safety

IO streams use standard Haskell idioms for resource safety. Since all
operations occur in the IO monad, you can use 'Control.Exception.catch',
'Control.Exception.bracket', or various \"@with...@\" functions to guard any
'System.IO.Streams.read' or 'System.IO.Streams.write' without any special
considerations:

@
import qualified "Data.ByteString" as S
import "System.IO"
import "System.IO.Streams" ('System.IO.Streams.InputStream', 'System.IO.Streams.OutputStream')
import qualified "System.IO.Streams" as Streams
import qualified "System.IO.Streams.File" as Streams

main =
    'System.IO.withFile' \"test.txt\" 'System.IO.ReadMode' $ \handle -> do
        stream <- Streams.'System.IO.Streams.handleToInputStream' handle
        mBytes <- Streams.'System.IO.Streams.read' stream
        case mBytes of
            'Just' bytes -> S.'Data.ByteString.putStrLn' bytes
            'Nothing'    -> 'System.IO.putStrLn' \"EOF\"
@

However, you can also simplify the above example by using the convenience
function 'System.IO.Streams.File.withFileAsInput' from
"System.IO.Streams.File":

@
'System.IO.Streams.withFileAsInput'
 :: 'System.IO.FilePath' -> ('System.IO.Streams.InputStream' 'Data.ByteString.ByteString' -> 'IO' a) -> 'IO' a
@

-}

{- $pushback

All 'System.IO.Streams.InputStream's support pushback, which simplifies many
types of operations. For example, we can 'System.IO.Streams.peek' at an
'System.IO.Streams.InputStream' by combining 'System.IO.Streams.read' and
'System.IO.Streams.unRead':

@
'System.IO.Streams.peek' :: 'System.IO.Streams.InputStream' c -> 'IO' ('Maybe' c)
'System.IO.Streams.peek' s = do
    x <- Streams.'System.IO.Streams.read' s
    case x of
        'Nothing' -> 'return' ()
        'Just' c  -> Streams.'System.IO.Streams.unRead' c s
    'return' x
@

... although "System.IO.Streams" already exports the above function.

'System.IO.Streams.InputStream's can customize pushback behavior to support
more sophisticated support for pushback. For example, if you protect a stream
using 'System.IO.Streams.throwIfProducesMoreThan' and
'System.IO.Streams.unRead' input, it will subtract the unread input from the
total byte count. However, these extra features will not interfere with the
basic pushback contract, given by the following law:

@
'System.IO.Streams.unRead' c stream >> 'System.IO.Streams.read' stream == 'return' ('Just' c)
@

When you build an 'System.IO.Streams.InputStream' using
'System.IO.Streams.makeInputStream', it supplies the default pushback behavior
which just saves the input for the next 'System.IO.Streams.read' call. More
advanced users can use "System.IO.Streams.Internal" to customize their own
pushback routines.

{- NOTE: The library only exports pushback API for Sources, which are a
         completely internal type, so should we teach the user how to define
         custom pushback or not?  Maybe that belongs in some sort of separate
         "advanced" tutorial for System.IO.Streams.Internal. -}
-}

{- $threadsafety

IO stream operations are not thread-safe by default for performance reasons.
However, you can transform an existing IO stream into a thread-safe one using
the provided locking functions:

@
'System.IO.Streams.lockingInputStream'  :: 'System.IO.Streams.InputStream'  a -> 'IO' ('System.IO.Streams.InputStream'  a)
'System.IO.Streams.lockingOutputStream' :: 'System.IO.Streams.OutputStream' a -> 'IO' ('System.IO.Streams.OutputStream' a)
@

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

{-# LANGUAGE BangPatterns #-}

-- | Stream primitives for decoding and encoding 'Text' values in UTF-8 format.
module System.IO.Streams.Text
  ( -- * Decoders and Encoders
    decodeUtf8
  , decodeUtf8With
  , encodeUtf8
  ) where

----------------------------------------------------------------------------
import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as S (length, null)
import qualified Data.ByteString.Unsafe        as S (unsafeDrop, unsafeIndex,
                                                     unsafeTake)
import           Data.Monoid                   (mappend)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as T (decodeUtf8,
                                                     decodeUtf8With,
                                                     encodeUtf8)
import           Data.Text.Encoding.Error      (OnDecodeError)
import           Data.Word                     (Word8)
------------------------------------------------------------------------------
import qualified System.IO.Streams.Combinators as Streams (contramap)
import           System.IO.Streams.Internal    (InputStream, OutputStream)
import qualified System.IO.Streams.Internal    as Streams (fromGenerator,
                                                           read, yield)


------------------------------------------------------------------------------
-- | Convert an 'OutputStream' taking 'ByteString's to an 'OutputStream' that
-- takes 'Text', encoding the data as UTF-8. See
-- @Data.Text.Encoding.'T.encodeUtf8'@.
encodeUtf8 :: OutputStream ByteString -> IO (OutputStream Text)
encodeUtf8 = Streams.contramap T.encodeUtf8


------------------------------------------------------------------------------
-- | Decode an 'InputStream' of 'ByteString's in UTF-8 format into an
-- 'InputStream' of 'Text' values. If decoding fails, will throw an exception.
-- See @Data.Text.Encoding.'T.decodeUtf8'@.
decodeUtf8 :: InputStream ByteString -> IO (InputStream Text)
decodeUtf8 = decode T.decodeUtf8
{-# INLINE decodeUtf8 #-}


------------------------------------------------------------------------------
-- | Decode an 'InputStream' of 'ByteString's in UTF-8 format into an
-- 'InputStream' of 'Text' values. If decoding fails, invokes the given
-- 'OnDecodeError' function to decide what to do. See
-- @Data.Text.Encoding.'T.decodeUtf8With'@.
decodeUtf8With :: OnDecodeError
               -> InputStream ByteString
               -> IO (InputStream Text)
decodeUtf8With e = decode (T.decodeUtf8With e)
{-# INLINE decodeUtf8With #-}


------------------------------------------------------------------------------
decode :: (ByteString -> Text)
       -> InputStream ByteString
       -> IO (InputStream Text)
decode decodeFunc input = Streams.fromGenerator $ go Nothing
  where
    go !soFar = liftIO (Streams.read input) >>=
                maybe (finish soFar) (chunk soFar)

    finish Nothing  = return $! ()
    finish (Just x) = Streams.yield $! decodeFunc x

    chunk Nothing  s = process s
    chunk (Just a) b = process $ a `mappend` b

    process !s =
      case findLastFullCode s of
        LastCodeIsComplete x -> (Streams.yield $! decodeFunc x) >> go Nothing
        Split a b            -> do
                                  when (not $ S.null a) $
                                      Streams.yield $! decodeFunc a
                                  go (Just b)
        NoCodesAreComplete x -> go (Just x)


------------------------------------------------------------------------------
data ByteType = Regular
              | Continuation
              | Start !Int


------------------------------------------------------------------------------
between :: Word8 -> Word8 -> Word8 -> Bool
between x y z = x >= y && x <= z
{-# INLINE between #-}


------------------------------------------------------------------------------
characterizeByte :: Word8 -> ByteType
characterizeByte c | between c 0 0x7F    = Regular
                   | between c 0x80 0xBF = Continuation
                   | between c 0xC0 0xDF = Start 1
                   | between c 0xE0 0xEF = Start 2
                   -- Technically utf-8 ends after 0xf4, but those sequences
                   -- won't decode anyways.
                   | otherwise           = Start 3


------------------------------------------------------------------------------
data FindOutput = LastCodeIsComplete !ByteString
                | Split !ByteString !ByteString
                | NoCodesAreComplete !ByteString   -- should be impossibly rare
                                                   -- in real data


------------------------------------------------------------------------------
findLastFullCode :: ByteString -> FindOutput
findLastFullCode b | len == 0  = LastCodeIsComplete b
                   | otherwise = go
  where
    len = S.length b

    go = let !idx = len - 1
             !c   = S.unsafeIndex b idx
         in case characterizeByte c of
              Regular      -> LastCodeIsComplete b
              Continuation -> cont (len - 2)
              _            -> Split (S.unsafeTake idx b) (S.unsafeDrop idx b)

    cont !idx | idx < 0 = NoCodesAreComplete b
              | otherwise =
                  let !c = S.unsafeIndex b idx
                  in case characterizeByte c of
                       -- what do we do with this? decoding will fail. give up
                       -- and lie, the text decoder will deal with it..
                       Regular      -> LastCodeIsComplete b
                       Continuation -> cont (idx - 1)
                       Start n      -> if n + idx == len - 1
                                         then LastCodeIsComplete b
                                         else Split (S.unsafeTake idx b)
                                                    (S.unsafeDrop idx b)
{-# INLINE findLastFullCode #-}

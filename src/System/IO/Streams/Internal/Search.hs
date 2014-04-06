{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module System.IO.Streams.Internal.Search
  ( search
  , MatchInfo(..)
  ) where

------------------------------------------------------------------------------
import           Control.Monad               (when)
import           Control.Monad.IO.Class      (liftIO)
import           Data.ByteString.Char8       (ByteString)
import qualified Data.ByteString.Char8       as S
import qualified Data.ByteString.Unsafe      as S
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Prelude                     hiding (read)
------------------------------------------------------------------------------
import           System.IO.Streams.Internal  (InputStream)
import qualified System.IO.Streams.Internal  as Streams


------------------------------------------------------------------------------
-- | 'MatchInfo' provides match information when performing string search.
data MatchInfo = Match   {-# UNPACK #-} !ByteString
               | NoMatch {-# UNPACK #-} !ByteString
  deriving (Show, Eq)


------------------------------------------------------------------------------
-- | Does the given needle match the haystack over the given ranges of indices?
matches :: ByteString     -- ^ needle
        -> Int            -- ^ needle start
        -> Int            -- ^ needle end (inclusive)
        -> ByteString     -- ^ haystack
        -> Int            -- ^ haystack start
        -> Int            -- ^ haystack end (inclusive)
        -> Bool
matches !needle !nstart !nend' !haystack !hstart !hend' =
    go nend' hend'
  where
    go !nend !hend =
        if nend < nstart || hend < hstart
          then True
          else let !nc = S.unsafeIndex needle nend
                   !hc = S.unsafeIndex haystack hend
               in if nc /= hc
                    then False
                    else go (nend-1) (hend-1)
{-# INLINE matches #-}


------------------------------------------------------------------------------
-- | Given a 'ByteString' to look for (the \"needle\") and an 'InputStream',
-- produces a new 'InputStream' which yields data of type 'MatchInfo'.
--
-- Example:
--
-- @
-- ghci> 'System.IO.Streams.fromList' [\"food\", \"oof\", \"oodles\", \"ok\"] >>=
--       'search' \"foo\" >>= 'System.IO.Streams.toList'
-- ['Match' \"foo\",'NoMatch' \"d\",'NoMatch' \"oo\",'Match' \"foo\",'NoMatch' \"dlesok\"]
-- @
--
-- Uses the Boyer-Moore-Horspool algorithm
-- (<http://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm>).
search :: ByteString                   -- ^ \"needle\" to look for
       -> InputStream ByteString       -- ^ input stream to wrap
       -> IO (InputStream MatchInfo)
search needle stream = Streams.fromGenerator $
                       lookahead nlen >>= either finishAndEOF startSearch

  where
    --------------------------------------------------------------------------
    finishAndEOF x = if S.null x
                       then return $! ()
                       else Streams.yield $! NoMatch x

    --------------------------------------------------------------------------
    startSearch !haystack =
        if S.null haystack
          then lookahead nlen >>= either finishAndEOF startSearch
          else go 0

      where
        ----------------------------------------------------------------------
        !hlen = S.length haystack

        ----------------------------------------------------------------------
        go !hidx
          | hend >= hlen = crossBound hidx
          | otherwise = do
              let match = matches needle 0 last haystack hidx hend
              if match
                then do
                  let !nomatch    = S.take hidx haystack
                  let !aftermatch = S.drop (hend + 1) haystack

                  produceMatch nomatch aftermatch
                else do
                  -- skip ahead
                  let c = S.unsafeIndex haystack hend
                  let !skip = V.unsafeIndex table $ fromEnum c
                  go (hidx + skip)

          where
            !hend = hidx + nlen - 1

        ----------------------------------------------------------------------
        mkCoeff hidx = let !ll = hlen - hidx
                           !nm = nlen - ll
                       in (ll, nm)

        ----------------------------------------------------------------------
        crossBound !hidx0 = do
            let (!leftLen, needMore) = mkCoeff hidx0

            lookahead needMore >>=
              either (\s -> finishAndEOF $ S.append haystack s)
                     (runNext hidx0 leftLen needMore)

          where
            runNext !hidx !leftLen !needMore !nextHaystack = do
                let match1 = matches needle leftLen last nextHaystack 0
                                     (needMore-1)
                let match2 = matches needle 0 (leftLen-1) haystack hidx
                                     (hlen-1)

                if match1 && match2
                  then do
                    let !nomatch = S.take hidx haystack
                    let !aftermatch = S.drop needMore nextHaystack

                    produceMatch nomatch aftermatch

                  else do
                    let c = S.unsafeIndex nextHaystack $ needMore - 1
                    let p = V.unsafeIndex table (fromEnum c)

                    if p < leftLen
                      then do
                        let !hidx' = hidx + p
                        let (!leftLen', needMore') = mkCoeff hidx'
                        let !nextlen = S.length nextHaystack
                        if nextlen < needMore'
                          then
                            -- this should be impossibly rare
                            lookahead (needMore' - nextlen) >>=
                              either (\s -> finishAndEOF $
                                            S.concat [ haystack
                                                     , nextHaystack
                                                     , s ])
                                     (\s -> runNext hidx' leftLen' needMore' $
                                            S.append nextHaystack s)
                          else runNext hidx' leftLen' needMore' nextHaystack
                      else do
                          let sidx = p - leftLen
                          let (!crumb, rest) = S.splitAt sidx nextHaystack
                          Streams.yield $! NoMatch $! S.append haystack crumb
                          startSearch rest

    --------------------------------------------------------------------------
    produceMatch nomatch aftermatch = do
        when (not $ S.null nomatch) $ Streams.yield $! NoMatch nomatch
        Streams.yield $! Match needle
        startSearch aftermatch

    --------------------------------------------------------------------------
    !nlen = S.length needle
    !last = nlen - 1

    --------------------------------------------------------------------------
    !table = V.create $ do
        t <- MV.replicate 256 nlen
        go t

      where
        go !t = go' 0
          where
            go' !i | i >= last  = return t
                   | otherwise = do
                let c = fromEnum $ S.unsafeIndex needle i
                MV.unsafeWrite t c (last - i)
                go' $! i+1

    --------------------------------------------------------------------------
    lookahead n = go id n
      where
        go dlist !k = liftIO (Streams.read stream) >>= maybe eof chunk
          where
            eof = return $! Left $! S.concat $ dlist []

            chunk x = if r <= 0
                        then return $! Right $! S.concat $ d' []
                        else go d' r
              where
                l  = S.length x
                r  = k - l
                d' = dlist . (x:)

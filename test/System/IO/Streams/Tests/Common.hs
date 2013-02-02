{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.IO.Streams.Tests.Common where

------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Exception
import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString          as S
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy     as L
import           Data.Typeable
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic  as QC


------------------------------------------------------------------------------
instance Arbitrary S.ByteString where
    arbitrary = liftM (S.pack . map c2w) arbitrary

instance Arbitrary L.ByteString where
    arbitrary = do
        n      <- choose(0,5)
        chunks <- replicateM n arbitrary
        return $ L.fromChunks chunks


------------------------------------------------------------------------------
eatException :: IO a -> IO ()
eatException a = (a >> return ()) `E.catch` handler
  where
    handler :: SomeException -> IO ()
    handler _ = return ()


------------------------------------------------------------------------------
forceSameType :: a -> a -> a
forceSameType _ a = a


------------------------------------------------------------------------------
-- | Kill the false negative on derived show instances.
coverShowInstance :: (MonadIO m, Show a) => a -> m ()
coverShowInstance x = liftIO (a >> b >> c)
  where
    a = eatException $ evaluate $ showsPrec 0 x ""
    b = eatException $ evaluate $ show x
    c = eatException $ evaluate $ showList [x] ""


------------------------------------------------------------------------------
coverReadInstance :: (MonadIO m, Read a) => a -> m ()
coverReadInstance x = do
    liftIO $ eatException $ evaluate $ forceSameType [(x,"")] $ readsPrec 0 ""
    liftIO $ eatException $ evaluate $ forceSameType [([x],"")] $ readList ""


------------------------------------------------------------------------------
coverEqInstance :: (Monad m, Eq a) => a -> m ()
coverEqInstance x = a `seq` b `seq` return ()
  where
    a = x == x
    b = x /= x


------------------------------------------------------------------------------
coverOrdInstance :: (Monad m, Ord a) => a -> m ()
coverOrdInstance x = a `deepseq` b `deepseq` return ()
  where
    a = [ x < x
        , x >= x
        , x > x
        , x <= x
        , compare x x == EQ ]

    b = min a $ max a a


------------------------------------------------------------------------------
coverTypeableInstance :: (Monad m, Typeable a) => a -> m ()
coverTypeableInstance a = typeOf a `seq` return ()


------------------------------------------------------------------------------
expectException :: IO a -> PropertyM IO ()
expectException m = do
    e <- liftQ $ try m
    case e of
      Left (z::SomeException)  -> (length $ show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


------------------------------------------------------------------------------
expectExceptionH :: IO a -> IO ()
expectExceptionH act = do
    e <- try act
    case e of
      Left (z::SomeException) -> (length $ show z) `seq` return ()
      Right _ -> fail "expected exception, didn't get one"


------------------------------------------------------------------------------
liftQ :: forall a m . (Monad m) => m a -> PropertyM m a
liftQ = QC.run

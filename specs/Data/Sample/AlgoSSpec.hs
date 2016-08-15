{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Sample.AlgoSSpec where


import qualified Control.Foldl           as F
import           Data.Foldable
import qualified Data.List               as L
import           System.Random.MWC
import           Test.Hspec
import           Test.QuickCheck         hiding (sample)
import           Test.QuickCheck.Monadic

import           Data.Sample.AlgoS


toDouble :: Int -> Double
toDouble = fromIntegral

spec :: Spec
spec = do
    describe "sample" $ do
        it "should return the input if the list is <N." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $
                    sample (2 * length xs) xs
                assert $ ss == xs

        it "should return a sample of the list if the list is >=N." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $
                    sample (floor $ toDouble (length xs) / 2) xs
                assert $ length ss < length xs

        it "should maintain the order of the input." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $
                    sample (floor $ toDouble (length xs) / 2) $ L.sort xs
                assert . all (uncurry (<=)) . zip ss $ tail ss

    describe "sampleStep" $ do
        it "should return the input if the list is <N." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $ \g ->
                    F.foldM (sampleInit (2 * length xs) g) xs
                assert $ ss == xs

        it "should return a sample of the list if the list is >=N." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $ \g ->
                    F.foldM (sampleInit (floor $ toDouble (length xs) / 2) g) xs
                assert $ length ss < length xs

        it "should maintain the order of the input." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $ \g ->
                    F.foldM (sampleInit (floor $ toDouble (length xs)) g)
                        $ L.sort xs
                assert . all (uncurry (<=)) . zip ss $ tail ss

        it "should accept the input incrementally." $
            property $ \(NonEmpty xs :: NonEmptyList Int) -> monadicIO $ do
                ss <- run . withSystemRandom . asGenIO $ \g ->
                    let s = sampleInit (floor $ toDouble (length xs) / 2) g
                    in  sampleDone =<< foldlM sampleStep s (L.sort xs)
                assert $ length ss < length xs
                assert . all (uncurry (<=)) . zip ss $ tail ss

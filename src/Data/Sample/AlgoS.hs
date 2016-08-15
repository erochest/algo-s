{-# LANGUAGE ScopedTypeVariables #-}


module Data.Sample.AlgoS where


import qualified Control.Foldl           as F
import           Control.Monad.Primitive
import qualified Data.HashMap.Strict     as M
import qualified Data.List               as L
import           Data.Ord
import           System.Random.MWC

import           Data.Sample.AlgoS.Types


sample :: (PrimMonad m, Foldable f) => Int -> f a -> Gen (PrimState m) -> m [a]
sample k xs g = F.foldM (sampleInit k g) xs

sampleInit :: PrimMonad m => Int -> Gen (PrimState m) -> Sampler m a
sampleInit k g = F.FoldM (foldStep g) start done
    where
        start = return (M.empty, 1.0)
        done  = return . fmap snd . L.sortBy (comparing fst) . M.toList . fst

        foldStep :: PrimMonad m
                 => Gen (PrimState m)
                 -> (M.HashMap Int a, Double)
                 -> a
                 -> m (M.HashMap Int a, Double)
        foldStep g' (m, n) a
            | n' <= k   = return (M.insert n' a m, n + 1.0)
            | otherwise = do
                x :: Double <- uniform g'
                if x <= (fromIntegral k / n)
                   then do
                       i <- (M.keys m !!) <$> uniformR (0, M.size m - 1) g'
                       return (M.insert n' a $ M.delete i m, n + 1.0)
                    else return (m, n + 1.0)
            where
                n' :: Int
                n' = floor n

sampleStep :: Monad m => Sampler m a -> a -> m (Sampler m a)
sampleStep = stepFoldM

sampleDone :: Monad m => Sampler m a -> m [a]
sampleDone (F.FoldM _ b z) = z =<< b

stepFold :: F.Fold a b -> a -> F.Fold a b
stepFold (F.Fold f b done) x = F.Fold f (f b x) done

stepFoldM :: Monad m => F.FoldM m a b -> a -> m (F.FoldM m a b)
stepFoldM (F.FoldM f b done) x = do
    b' <- b
    return $ F.FoldM f (f b' x) done

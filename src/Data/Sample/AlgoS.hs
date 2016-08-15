module Data.Sample.AlgoS where


-- import Data.Foldable
import System.Random.MWC
import Control.Monad.Primitive

import Data.Sample.AlgoS.Types


sample :: (PrimMonad m, Foldable f) => Int -> f a -> Gen (PrimState m) -> m [a]
sample = undefined

sampleInit :: PrimMonad m => Int -> Gen (PrimState m) -> Sampler m a
sampleInit = undefined

sampleStep :: Monad m => Sampler m a -> a -> m (Sampler m a)
sampleStep = undefined

sampleDone :: Monad m => Sampler m a -> m [a]
sampleDone = undefined



module Data.Sample.AlgoS.Types where


import           Control.Foldl (FoldM)


type Sampler m a = FoldM m a [a]

module Types where


-- import           Data.Sample.AlgoS.Types


data Actions = Sample 
             { sampleN :: !Int
             }
             deriving (Show, Eq)

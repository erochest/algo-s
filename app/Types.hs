module Types where


-- import           Data.Sample.AlgoS.Types


data Actions
    = Sample { defaultOutput :: !(Maybe FilePath)
             , defaultInput  :: !(Maybe FilePath)
             }
    deriving (Show, Eq)

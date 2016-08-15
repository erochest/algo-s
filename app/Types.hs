module Types where


-- import           Data.Sample.AlgoS.Types


data Actions
        = Default { defaultOutput :: !FilePath
                  , defaultInput  :: !FilePath
                  }
        deriving (Show, Eq)
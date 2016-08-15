-- {-# LANGUAGE DeriveDataTypeable         #-}
-- {-# LANGUAGE DeriveFunctor              #-}
-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE DeriveTraversable          #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedLists            #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE TemplateHaskell            #-}


module Data.Sample.AlgoS.Types where


-- import           Control.Lens
-- import           Data.Data
-- import qualified Data.Text              as T
-- import           GHC.Generics           hiding (to)

import           Control.Foldl (FoldM)


type Sampler m a = FoldM m a [a]

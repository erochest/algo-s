{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.Random.MWC

import           Data.Sample.AlgoS

import           Types


action :: Actions -> Script ()
action Sample{..} = scriptIO . withSystemRandom . asGenIO $ \g ->
    L8.getContents
        >>= flip (sample sampleN) g . L8.lines
        >>= L8.putStr . L8.unlines

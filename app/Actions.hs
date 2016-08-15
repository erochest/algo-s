{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           Data.Sample.AlgoS.Actions.Default

import           Types


action :: Actions -> Script ()

action Default{..} = defaultAction defaultInput defaultOutput

{-# LANGUAGE LambdaCase #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


-- import           Control.Monad       (mzero)
-- import qualified Data.List           as L
-- import qualified Data.Text           as T
import           Options.Applicative

-- import           Data.Sample.AlgoS.Types

import           Types


-- textOption :: Mod OptionFields T.Text -> Parser T.Text
-- textOption = option (T.pack <$> str)

outputOpt :: Parser FilePath
outputOpt = strOption (  short 'o' <> long "output" <> metavar "OUTPUT_FILE"
                      <> help "The file to write back to.")

inputOpt :: Parser FilePath
inputOpt = strOption (  short 'i' <> long "input" <> metavar "INPUT_FILE"
                     <> help "The input file to process.")

-- inputsOpt :: Parser [FilePath]
-- inputsOpt = many (strArgument (  metavar "INPUT_FILES ..."
                              -- <> help "Input data files."))

defaultOpts :: Parser Actions
defaultOpts = Default <$> outputOpt <*> inputOpt

opts' :: Parser Actions
opts' = subparser
      (  command "default" (info (helper <*> defaultOpts)
                          (progDesc "Default action and options."))
      )

opts :: ParserInfo Actions
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "An implementaiton of Knuth's algorithm S (https://rosettacode.org/wiki/Knuth%27s_algorithm_S) with a command-line utility for sampling from STDIN."
            <> header "algo-s - An implementation of Knuth's algorithm S.")

parseOpts :: IO Actions
parseOpts = execParser opts
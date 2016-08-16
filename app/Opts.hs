{-# LANGUAGE LambdaCase #-}


module Opts
    ( Actions(..)
    , opts
    , execParser
    , parseOpts
    ) where


import           Options.Applicative

import           Types


defaultOpts :: Parser Actions
defaultOpts =   Sample
            <$> option auto (  short 'n' <> long "n"
                            <> value 1000 <> metavar "SAMPLE_SIZE"
                            <> help "The number of items to return in the\
                                    \ sample. Defaults to 1,000.")

opts :: ParserInfo Actions
opts = info (helper <*> defaultOpts)
            (  fullDesc
            <> progDesc "An implementaiton of Knuth's algorithm S\
                        \ (https://rosettacode.org/wiki/Knuth%27s_algorithm_S)\
                        \ with a command-line utility for sampling from STDIN."
            <> header "algo-s - An implementation of Knuth's algorithm S.")

parseOpts :: IO Actions
parseOpts = execParser opts

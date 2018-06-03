{-|
Module      : Args
Description : Command line argument parsing.

Functions and utilities to parse the arguments from the command line.
 -}

module Args
  ( -- * parse the command line arguments
    parseArgs
    -- * structure representing the arguments
  , Args(..)
  ) where

import Options.Applicative

data Args = Args
  { coeffs :: [Int] -- ^ coefficients to generate the sum
  , start  :: Int   -- ^ from where to start the sum
  , end    :: Int   -- ^ where to end the summation
  -- TODO: image related arguments
  } deriving Show

parseArgs :: IO Args
parseArgs = execParser $ info (helper <*> parseArgsInternal) buildMod

buildMod :: InfoMod a
buildMod = mconcat
  [ fullDesc
  , header "Exponential sum of the day"
  ]

parseArgsInternal :: Parser Args
parseArgsInternal = Args
  <$> parseCoefs
  <*> parseStart
  <*> parseEnd

parseCoefs :: Parser [Int]
parseCoefs = many $ argument auto $ mconcat
  [ metavar "COEFFICIENT"
  , help "Coefficient of the exponential argument"
  ]

parseStart :: Parser Int
parseStart = option auto $ mconcat
  [ long "start"
  , short 'a'
  , metavar "INDEX"
  , help "Start summing from INDEX"
  , value 0
  , showDefault
  ]

parseEnd :: Parser Int
parseEnd = option auto $ mconcat
  [ long "end"
  , short 'b'
  , metavar "INDEX"
  , help "Sum only until INDEX"
  , value 10000
  , showDefault
  ]

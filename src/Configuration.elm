module Configuration exposing (..)

import String exposing (fromList)

type alias Configuration =
  { guesses : GuessLength
  , patternLen : PatternLength
  , colorCount : Int
  , duplicates : Bool
  }

type GuessLength
  = G6
  | G8
  | G10
  | G12

guessToInt : GuessLength -> Int
guessToInt g =
  case g of
    G6 -> 6
    G8 -> 8
    G10 -> 10
    G12 -> 12

guessToChar : GuessLength -> Char
guessToChar g =
  case g of
    G6 -> '6'
    G8 -> '8'
    G10 -> 'a'
    G12 -> 'c'

type PatternLength
  = P4
  | P5
  | P6
  | P7
  | P8

patternLenToInt : PatternLength -> Int
patternLenToInt p =
  case p of
    P4 -> 4
    P5 -> 5
    P6 -> 6
    P7 -> 7
    P8 -> 8

patternLenToChar : PatternLength -> Char
patternLenToChar p =
  case p of
    P4 -> '4'
    P5 -> '5'
    P6 -> '6'
    P7 -> '7'
    P8 -> '8'

default : Configuration
default =
  { guesses = G10
  , patternLen = P4
  , colorCount = 6
  , duplicates = True
  -- , seed = Nothing
  }

serializeForUrl : Configuration -> String
serializeForUrl cfg =
  let
    guessLen : Char
    guessLen = guessToChar cfg.guesses

    patternLen : Char
    patternLen = patternLenToChar cfg.patternLen
  in
    fromList [guessLen, patternLen]

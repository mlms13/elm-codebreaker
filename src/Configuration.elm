module Configuration exposing (..)

type alias Configuration =
  { guesses : Int
  , patternLen : Int
  , colorCount : Int
  , duplicates : Bool
  }

default : Configuration
default =
  { guesses = 10
  , patternLen = 4
  , colorCount = 6
  , duplicates = True
  }

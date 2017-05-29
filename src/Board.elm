module Board exposing (..)

import List exposing (repeat)
import Maybe exposing (map)
import Maybe.Extra exposing (combine)
import Configuration exposing (Configuration)
import GamePiece exposing (..)

type alias Pattern = List GamePiece
type alias Outcome =
  { perfect : Int -- number of colors in exactly the right place
  , almost : Int -- number of colors in the wrong place
  }

type alias Board =
  { correct : Pattern
  , turns : List (Pattern, Outcome)
  , current : List (Maybe GamePiece)
  }

-- TODO: should also take a seed for the RNG
-- TODO: should slice the list of possible pieces
create : Configuration -> Board
create cfg =
  { correct = repeat cfg.patternLen Red
  , turns = []
  , current = repeat cfg.patternLen Nothing
  }

-- looks at a board, and if all of `current` is filled,
-- compares that `current` against `correct`
boardToOutcome : Board -> Maybe Outcome
boardToOutcome b =
  b.current |> combine |> map (\p -> (comparePattern b.correct p))

comparePattern : Pattern -> Pattern -> Outcome
comparePattern correct check =
  { perfect = 0
  , almost = 0
  }

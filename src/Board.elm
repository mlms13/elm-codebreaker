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
  { answer : Pattern
  , turns : List (Pattern, Outcome)
  , current : List (Maybe GamePiece)
  }

-- TODO: should also take a seed for the RNG
-- TODO: should slice the list of possible pieces
create : Configuration -> Board
create cfg =
  { answer = repeat cfg.patternLen Red
  , turns = []
  , current = repeat cfg.patternLen Nothing
  }

-- looks at a board, and if all of `current` is filled,
-- compares that `current` against `answer`
boardToOutcome : Board -> Maybe Outcome
boardToOutcome b =
  b.current |> combine |> map (\p -> (comparePattern b.answer p))

comparePattern : Pattern -> Pattern -> Outcome
comparePattern answer check =
  { perfect = 0
  , almost = 0
  }

advanceTurn : (Pattern, Outcome) -> Board -> Board
advanceTurn (pattern, outcome) b =
  { answer = b.answer
  , turns = b.turns ++ [(pattern, outcome)]
  , current = repeat (List.length pattern) Nothing
  }


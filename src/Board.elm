module Board exposing (..)

import Dict exposing (Dict)
import List exposing (repeat, foldl)
import List.Extra exposing (zip)
import Maybe exposing (map)
import Maybe.Extra exposing (combine, unwrap)
import Random exposing (Generator, Seed)
import Configuration exposing (Configuration)
import GamePiece exposing (..)

type alias Pattern =
  List GamePiece

type alias Outcome =
  { perfect : Int -- number of colors in exactly the right place
  , almost : Int -- number of colors in the wrong place
  }

type alias OutcomeHash =
  (Dict String Int, Outcome)

type alias Board =
  { answer : Pattern
  , turns : List (Pattern, Outcome)
  , current : List (Maybe GamePiece)
  }

-- create a board from the current configuration,
-- using the provided seed and a sliced list of colors
-- based on config colorCount
create : Configuration -> Seed -> Board
create cfg seed =
  let
    patternLen : Int
    patternLen =
      Configuration.patternLenToInt cfg.patternLen
  in
    { answer = randomPattern cfg seed
    , turns = []
    , current = repeat patternLen Nothing
    }

randomPattern : Configuration -> Seed -> Pattern
randomPattern cfg seed =
  let
    patternLen : Int
    patternLen =
      Configuration.patternLenToInt cfg.patternLen

    listGenerator : Generator Pattern
    listGenerator =
      Random.list patternLen (generatePiece cfg.colorCount)

    -- generated : (Pattern, Seed)
    (pattern, _) =
      Random.step listGenerator seed
  in
    pattern

emptyOutcome : Outcome
emptyOutcome =
  { perfect = 0
  , almost = 0
  }

-- looks at a board, and if all of `current` is filled,
-- compares that `current` against `answer`
boardToOutcome : Board -> Maybe Outcome
boardToOutcome b =
  b.current |> combine |> map (\p -> (comparePattern b.answer p))


-- if `hash` at `first` is negative, increment `almost`
-- either way, increment `hash` at `first`
-- if `hash` at `second` is positive, increment `almost`
-- either way, decrement `hash` at `second`
compareNonMatching : (GamePiece, GamePiece) -> OutcomeHash -> OutcomeHash
compareNonMatching (first, second) (hash, outcome) =
  let
    showFirst : String
    showFirst =
      show first

    showSecond : String
    showSecond =
      show second

    incrementOrOne : Maybe Int -> Maybe Int
    incrementOrOne v =
      Just (unwrap 1 (\int -> int + 1) v)

    decrementOrNegativeOne : Maybe Int -> Maybe Int
    decrementOrNegativeOne v =
      Just (unwrap -1 (\int -> int - 1) v)

    -- the dict value for the first piece always gets incremented
    hashUpdateFirst : Dict String Int -> Dict String Int
    hashUpdateFirst d =
      Dict.update showFirst incrementOrOne d

    -- the dict value for the first piece always gets decremented
    hashUpdateSecond : Dict String Int -> Dict String Int
    hashUpdateSecond d =
      Dict.update showSecond decrementOrNegativeOne d

    -- if the current hash has a negative (or 0) value in the position
    -- of our first piece, we must have incremented it, so we
    -- should increase the "almost" value by 1
    almostIncrementFirst : Dict String Int -> Int
    almostIncrementFirst d =
      if (Maybe.withDefault 0 (Dict.get showFirst d)) <= 0 then
        1
      else
        0

    -- similar to above, but with the signs switched, because we
    -- decrement for the second piece, so a positive value means
    -- we just decremented
    almostIncrementSecond : Dict String Int -> Int
    almostIncrementSecond d =
      if (Maybe.withDefault 0 (Dict.get showSecond d)) >= 0 then
        1
      else
        0

    -- plus one if hash at first is negative or zero
    -- plus one if hash at second is positive or zero
    almostIncrement : Dict String Int -> Int
    almostIncrement d =
      (almostIncrementFirst d) + (almostIncrementSecond d)

    updatedHash : Dict String Int
    updatedHash =
      hashUpdateFirst (hashUpdateSecond hash)
  in
    ( updatedHash
    , { outcome | almost = outcome.almost + (almostIncrement updatedHash) }
    )

comparePattern : Pattern -> Pattern -> Outcome
comparePattern a b =
  let
    isPos : Maybe Int -> Bool
    isPos v =
      unwrap False (\i -> i > 0) v

    isNeg : Maybe Int -> Bool
    isNeg v =
      unwrap False (\i -> i < 0) v

    checkOne : (GamePiece, GamePiece) -> OutcomeHash -> OutcomeHash
    checkOne (first, second) (hash, outcome) =
      if first == second then
        (hash, { outcome | perfect = outcome.perfect + 1 })
      else
        compareNonMatching (first, second) (hash, outcome)

    checkAll : List (GamePiece, GamePiece) -> OutcomeHash
    checkAll zipped =
      foldl checkOne (Dict.empty, emptyOutcome) zipped
  in
    checkAll (zip a b) |> Tuple.second

advanceTurn : (Pattern, Outcome) -> Board -> Board
advanceTurn (pattern, outcome) b =
  { answer = b.answer
  , turns = b.turns ++ [(pattern, outcome)]
  , current = repeat (List.length pattern) Nothing
  }


module TestBoard exposing (..)

import Test exposing (..)
import Expect
import Board exposing(..)
import GamePiece exposing(..)

rrrr : List GamePiece
rrrr =
  [Red, Red, Red, Red]

gggg : List GamePiece
gggg =
  [Green, Green, Green, Green]

gggr : List GamePiece
gggr =
  [Green, Green, Green, Red]

roygb : List GamePiece
roygb =
  [Red, Orange, Yellow, Green, Blue]

bgyor : List GamePiece
bgyor =
  [Blue, Green, Yellow, Orange, Red]

rrgg : List GamePiece
rrgg =
  [Red, Red, Green, Green]

rgrg : List GamePiece
rgrg =
  [Red, Green, Red, Green]

suite : Test
suite =
  describe "Test Board-related utilities"
    [ describe "Test comparing patterns"
      [ test "RRRR vs RRRR" <|
        \() ->
          Expect.equal
            { perfect = 4, almost = 0 }
            (comparePattern rrrr rrrr)

      , test "RRRR vs GGGG" <|
        \() ->
          Expect.equal
            { perfect = 0, almost = 0 }
            (comparePattern rrrr gggg)

      , test "RRRR vs GGGR" <|
        \() ->
          Expect.equal
            { perfect = 1, almost = 0 }
            (comparePattern rrrr gggr)

      , test "ROYGB vs BGYOR" <|
        \() ->
          Expect.equal
            { perfect = 1, almost = 4 }
            (comparePattern roygb bgyor)
      , test "RRGG vs RGRG" <|
        \() ->
          Expect.equal
            { perfect = 2, almost = 2 }
            (comparePattern rrgg rgrg)
      ]
    ]

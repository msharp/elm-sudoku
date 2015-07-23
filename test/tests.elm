
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import Dict
import Sudoku 

board1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
board1_parsed =
  [("A1","0"),("A2","0"),("A3","3"),("A4","0"),("A5","2"),("A6","0"),("A7","6"),("A8","0"),("A9","0")
  ,("B1","9"),("B2","0"),("B3","0"),("B4","3"),("B5","0"),("B6","5"),("B7","0"),("B8","0"),("B9","1")
  ,("C1","0"),("C2","0"),("C3","1"),("C4","8"),("C5","0"),("C6","6"),("C7","4"),("C8","0"),("C9","0")
  ,("D1","0"),("D2","0"),("D3","8"),("D4","1"),("D5","0"),("D6","2"),("D7","9"),("D8","0"),("D9","0")
  ,("E1","7"),("E2","0"),("E3","0"),("E4","0"),("E5","0"),("E6","0"),("E7","0"),("E8","0"),("E9","8")
  ,("F1","0"),("F2","0"),("F3","6"),("F4","7"),("F5","0"),("F6","8"),("F7","2"),("F8","0"),("F9","0")
  ,("G1","0"),("G2","0"),("G3","2"),("G4","6"),("G5","0"),("G6","9"),("G7","5"),("G8","0"),("G9","0")
  ,("H1","8"),("H2","0"),("H3","0"),("H4","2"),("H5","0"),("H6","3"),("H7","0"),("H8","0"),("H9","9")
  ,("I1","0"),("I2","0"),("I3","5"),("I4","0"),("I5","1"),("I6","0"),("I7","3"),("I8","0"),("I9","0")
  ]

boardTests : Test
boardTests = suite "Sudoku Board Test Suite"
        [ test "Squares" (assertEqual 81 (List.length Sudoku.squares))
        , test "Unit list" (assertEqual 27 (List.length Sudoku.unitlist))
        , test "3 units per square" (assertEqual 3 (List.length (Sudoku.getUnits "G2")))
        , test "20 peers per square" (assertEqual 20 (List.length (Sudoku.getPeers "C5")))
        , test "Contents of C2 units" (assertEqual (Sudoku.getUnits "C2") [["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"],
                                                                            ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "I2"],
                                                                            ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"]])
        , test "Contents of C2 peers" (assertEqual (Sudoku.getPeers "C2") ["A1","A3","B1","B3","A2","B2","D2","E2","F2","G2",
                                                                            "H2","I2","C1","C3","C4","C5","C6","C7","C8","C9"] )


        , test "Parse a board string" (assertEqual (Sudoku.gridValues board1) (Dict.fromList board1_parsed))
        ]


main = runDisplay boardTests


import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import Dict
import Sudoku 

board1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
board1_parsed = 
  [("A1", Nothing),("A2", Nothing),("A3", Just "3"),("A4", Nothing),("A5", Just "2"),("A6", Nothing),("A7", Just "6"),("A8", Nothing),("A9", Nothing)
  ,("B1", Just "9"),("B2", Nothing),("B3", Nothing),("B4", Just "3"),("B5", Nothing),("B6", Just "5"),("B7", Nothing),("B8", Nothing),("B9", Just "1")
  ,("C1", Nothing),("C2", Nothing),("C3", Just "1"),("C4", Just "8"),("C5", Nothing),("C6", Just "6"),("C7", Just "4"),("C8", Nothing),("C9", Nothing)
  ,("D1", Nothing),("D2", Nothing),("D3", Just "8"),("D4", Just "1"),("D5", Nothing),("D6", Just "2"),("D7", Just "9"),("D8", Nothing),("D9", Nothing)
  ,("E1", Just "7"),("E2", Nothing),("E3", Nothing),("E4", Nothing),("E5", Nothing),("E6", Nothing),("E7", Nothing),("E8", Nothing),("E9", Just "8")
  ,("F1", Nothing),("F2", Nothing),("F3", Just "6"),("F4", Just "7"),("F5", Nothing),("F6", Just "8"),("F7", Just "2"),("F8", Nothing),("F9", Nothing)
  ,("G1", Nothing),("G2", Nothing),("G3", Just "2"),("G4", Just "6"),("G5", Nothing),("G6", Just "9"),("G7", Just "5"),("G8", Nothing),("G9", Nothing)
  ,("H1", Just "8"),("H2", Nothing),("H3", Nothing),("H4", Just "2"),("H5", Nothing),("H6", Just "3"),("H7", Nothing),("H8", Nothing),("H9", Just "9")
  ,("I1", Nothing),("I2", Nothing),("I3", Just "5"),("I4", Nothing),("I5", Just "1"),("I6", Nothing),("I7", Just "3"),("I8", Nothing),("I9", Nothing)
  ]
board1_allocated = 
  [("A1", "123456789"),("A2", "123456789"),("A3", "3"),("A4", "123456789"),("A5", "2"),("A6", "123456789"),("A7", "6"),("A8", "123456789"),("A9", "123456789")
  ,("B1", "9"),("B2", "123456789"),("B3", "123456789"),("B4", "3"),("B5", "123456789"),("B6", "5"),("B7", "123456789"),("B8", "123456789"),("B9", "1")
  ,("C1", "123456789"),("C2", "123456789"),("C3", "1"),("C4", "8"),("C5", "123456789"),("C6", "6"),("C7", "4"),("C8", "123456789"),("C9", "123456789")
  ,("D1", "123456789"),("D2", "123456789"),("D3", "8"),("D4", "1"),("D5", "123456789"),("D6", "2"),("D7", "9"),("D8", "123456789"),("D9", "123456789")
  ,("E1", "7"),("E2", "123456789"),("E3", "123456789"),("E4", "123456789"),("E5", "123456789"),("E6", "123456789"),("E7", "123456789"),("E8", "123456789"),("E9", "8")
  ,("F1", "123456789"),("F2", "123456789"),("F3", "6"),("F4", "7"),("F5", "123456789"),("F6", "8"),("F7", "2"),("F8", "123456789"),("F9", "123456789")
  ,("G1", "123456789"),("G2", "123456789"),("G3", "2"),("G4", "6"),("G5", "123456789"),("G6", "9"),("G7", "5"),("G8", "123456789"),("G9", "123456789")
  ,("H1", "8"),("H2", "123456789"),("H3", "123456789"),("H4", "2"),("H5", "123456789"),("H6", "3"),("H7", "123456789"),("H8", "123456789"),("H9", "9")
  ,("I1", "123456789"),("I2", "123456789"),("I3", "5"),("I4", "123456789"),("I5", "1"),("I6", "123456789"),("I7", "3"),("I8", "123456789"),("I9", "123456789")
  ]


boardTests : Test
boardTests = suite "Sudoku Board Test Suite"
        [ test "Squares" (assertEqual 81 (List.length Sudoku.squares))
        , test "Unit list" (assertEqual 27 (List.length Sudoku.unitlist))
        , test "3 units per square" (assertEqual 3 (List.length (Sudoku.getUnits "G2")))
        , test "20 peers per square" (assertEqual 20 (List.length (Sudoku.getPeers "C5")))
        , test "Contents of C2 units" (assertEqual (Sudoku.getUnits "C2") [["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"]
                                                                          ,["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "I2"]
                                                                          ,["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"]
                                                                          ])
        , test "Contents of C2 peers" (assertEqual (Sudoku.getPeers "C2") ["A1","A3","B1","B3","A2","B2","D2","E2","F2","G2"
                                                                          ,"H2","I2","C1","C3","C4","C5","C6","C7","C8","C9"
                                                                          ])

        , test "Parse a board string" (assertEqual (Sudoku.gridValues board1) (Dict.fromList board1_parsed))
        , test "Initially allocate possible known values" (assertEqual (Sudoku.parseGrid board1) (Dict.fromList board1_allocated))
        ]


main = runDisplay boardTests

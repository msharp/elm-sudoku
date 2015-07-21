
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import Dict
import Sudoku 


tests : Test
tests = suite "Sudoku Board Test Suite"
        [ test "Squares" (assertEqual 81 (List.length Sudoku.squares))
        , test "Unit list" (assertEqual 27 (List.length Sudoku.unitlist))
        , test "3 units per square" (assertEqual 3 (List.length (Sudoku.getUnits "G2")))
        , test "20 peers per square" (assertEqual 20 (List.length (Sudoku.getPeers "C5")))
        , test "Contents of C2 units" (assertEqual (Sudoku.getUnits "C2") [["A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"],
                                                                            ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2", "I2"],
                                                                            ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"]])
        , test "Contents of C2 peers" (assertEqual (Sudoku.getPeers "C2") ["A1","A3","B1","B3","A2","B2","D2","E2","F2","G2",
                                                                            "H2","I2","C1","C3","C4","C5","C6","C7","C8","C9"] )
        ]


main = runDisplay tests

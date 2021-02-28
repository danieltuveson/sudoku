module GridTest(gridTests) where 

import Test.HUnit
import Grid
import Validation 
import Solve
import Parse 

-- taken from https://www.websudoku.com/?level=1&set_id=141215464
testGrid :: Grid 
testGrid = Grid 
    [
        (Cell (Point 1 1) $ Just 2), (Cell (Point 1 2) Nothing), (Cell (Point 1 3) $ Just 5), (Cell (Point 1 4) Nothing), (Cell (Point 1 5) $ Just 3), (Cell (Point 1 6) Nothing), (Cell (Point 1 7) $ Just 1), (Cell (Point 1 8) Nothing), (Cell (Point 1 9) $ Just 8), 
        (Cell (Point 2 1) Nothing), (Cell (Point 2 2) Nothing), (Cell (Point 2 3) $ Just 9), (Cell (Point 2 4) $ Just 8), (Cell (Point 2 5) Nothing), (Cell (Point 2 6) Nothing), (Cell (Point 2 7) Nothing), (Cell (Point 2 8) $ Just 2), (Cell (Point 2 9) Nothing), 
        (Cell (Point 3 1) Nothing), (Cell (Point 3 2) $ Just 8), (Cell (Point 3 3) Nothing), (Cell (Point 3 4) $ Just 1), (Cell (Point 3 5) $ Just 7), (Cell (Point 3 6) Nothing), (Cell (Point 3 7) Nothing), (Cell (Point 3 8) Nothing), (Cell (Point 3 9) Nothing), 
        (Cell (Point 4 1) Nothing), (Cell (Point 4 2) $ Just 1), (Cell (Point 4 3) $ Just 2), (Cell (Point 4 4) Nothing), (Cell (Point 4 5) $ Just 8), (Cell (Point 4 6) Nothing), (Cell (Point 4 7) Nothing), (Cell (Point 4 8) $ Just 9), (Cell (Point 4 9) Nothing), 
        (Cell (Point 5 1) $ Just 9), (Cell (Point 5 2) Nothing), (Cell (Point 5 3) $ Just 7), (Cell (Point 5 4) Nothing), (Cell (Point 5 5) $ Just 6), (Cell (Point 5 6) Nothing), (Cell (Point 5 7) $ Just 4), (Cell (Point 5 8) Nothing), (Cell (Point 5 9) $ Just 1), 
        (Cell (Point 6 1) Nothing), (Cell (Point 6 2) $ Just 6), (Cell (Point 6 3) Nothing), (Cell (Point 6 4) Nothing), (Cell (Point 6 5) $ Just 4), (Cell (Point 6 6) Nothing), (Cell (Point 6 7) $ Just 2), (Cell (Point 6 8) $ Just 3), (Cell (Point 6 9) Nothing), 
        (Cell (Point 7 1) Nothing), (Cell (Point 7 2) Nothing), (Cell (Point 7 3) Nothing), (Cell (Point 7 4) Nothing), (Cell (Point 7 5) $ Just 1), (Cell (Point 7 6) $ Just 7), (Cell (Point 7 7) Nothing), (Cell (Point 7 8) $ Just 6), (Cell (Point 7 9) Nothing), 
        (Cell (Point 8 1) Nothing), (Cell (Point 8 2) $ Just 9), (Cell (Point 8 3) Nothing), (Cell (Point 8 4) Nothing), (Cell (Point 8 5) Nothing), (Cell (Point 8 6) $ Just 8), (Cell (Point 8 7) $ Just 7), (Cell (Point 8 8) Nothing), (Cell (Point 8 9) Nothing), 
        (Cell (Point 9 1) $ Just 7), (Cell (Point 9 2) Nothing), (Cell (Point 9 3) $ Just 3), (Cell (Point 9 4) Nothing), (Cell (Point 9 5) $ Just 9), (Cell (Point 9 6) Nothing), (Cell (Point 9 7) $ Just 8), (Cell (Point 9 8) Nothing), (Cell (Point 9 9) $ Just 4)
    ]

-- what updateGridActual should return
updateGridExpected :: Grid 
updateGridExpected = Grid 
    [
        (Cell (Point 1 1) $ Just 9), (Cell (Point 1 2) Nothing), (Cell (Point 1 3) $ Just 5), (Cell (Point 1 4) Nothing), (Cell (Point 1 5) $ Just 3), (Cell (Point 1 6) Nothing), (Cell (Point 1 7) $ Just 1), (Cell (Point 1 8) Nothing), (Cell (Point 1 9) $ Just 8), 
        (Cell (Point 2 1) Nothing), (Cell (Point 2 2) Nothing), (Cell (Point 2 3) $ Just 9), (Cell (Point 2 4) $ Just 8), (Cell (Point 2 5) Nothing), (Cell (Point 2 6) Nothing), (Cell (Point 2 7) Nothing), (Cell (Point 2 8) $ Just 2), (Cell (Point 2 9) Nothing), 
        (Cell (Point 3 1) Nothing), (Cell (Point 3 2) $ Just 8), (Cell (Point 3 3) Nothing), (Cell (Point 3 4) $ Just 5), (Cell (Point 3 5) $ Just 7), (Cell (Point 3 6) Nothing), (Cell (Point 3 7) Nothing), (Cell (Point 3 8) Nothing), (Cell (Point 3 9) Nothing), 
        (Cell (Point 4 1) Nothing), (Cell (Point 4 2) $ Just 1), (Cell (Point 4 3) $ Just 2), (Cell (Point 4 4) Nothing), (Cell (Point 4 5) $ Just 8), (Cell (Point 4 6) Nothing), (Cell (Point 4 7) Nothing), (Cell (Point 4 8) $ Just 9), (Cell (Point 4 9) Nothing), 
        (Cell (Point 5 1) $ Just 9), (Cell (Point 5 2) Nothing), (Cell (Point 5 3) $ Just 7), (Cell (Point 5 4) Nothing), (Cell (Point 5 5) $ Just 6), (Cell (Point 5 6) Nothing), (Cell (Point 5 7) $ Just 4), (Cell (Point 5 8) Nothing), (Cell (Point 5 9) $ Just 1), 
        (Cell (Point 6 1) Nothing), (Cell (Point 6 2) $ Just 6), (Cell (Point 6 3) Nothing), (Cell (Point 6 4) Nothing), (Cell (Point 6 5) $ Just 4), (Cell (Point 6 6) Nothing), (Cell (Point 6 7) $ Just 2), (Cell (Point 6 8) $ Just 3), (Cell (Point 6 9) Nothing), 
        (Cell (Point 7 1) Nothing), (Cell (Point 7 2) Nothing), (Cell (Point 7 3) Nothing), (Cell (Point 7 4) Nothing), (Cell (Point 7 5) $ Just 1), (Cell (Point 7 6) $ Just 7), (Cell (Point 7 7) Nothing), (Cell (Point 7 8) $ Just 6), (Cell (Point 7 9) Nothing), 
        (Cell (Point 8 1) Nothing), (Cell (Point 8 2) $ Just 9), (Cell (Point 8 3) Nothing), (Cell (Point 8 4) Nothing), (Cell (Point 8 5) Nothing), (Cell (Point 8 6) $ Just 8), (Cell (Point 8 7) $ Just 7), (Cell (Point 8 8) $ Just 2), (Cell (Point 8 9) Nothing), 
        (Cell (Point 9 1) $ Just 7), (Cell (Point 9 2) Nothing), (Cell (Point 9 3) $ Just 3), (Cell (Point 9 4) Nothing), (Cell (Point 9 5) $ Just 9), (Cell (Point 9 6) Nothing), (Cell (Point 9 7) $ Just 8), (Cell (Point 9 8) Nothing), (Cell (Point 9 9) $ Just 5)
    ]

updateGridActual :: Grid
updateGridActual = 
    let ins1 = updateState testGrid (Point 3 4) 5
        ins2 = updateState ins1 (Point 1 1) 9
        ins3 = updateState ins2 (Point 9 9) 5
        ins4 = updateState ins3 (Point 8 8) 2
    in ins4



-- full solution to testGrid
testSolution :: Maybe Grid 
testSolution = Just $ Grid 
    [
        (Cell (Point 1 1) $ Just 2), (Cell (Point 1 2) $ Just 4), (Cell (Point 1 3) $ Just 5), (Cell (Point 1 4) $ Just 6), (Cell (Point 1 5) $ Just 3), (Cell (Point 1 6) $ Just 9), (Cell (Point 1 7) $ Just 1), (Cell (Point 1 8) $ Just 7), (Cell (Point 1 9) $ Just 8),
        (Cell (Point 2 1) $ Just 1), (Cell (Point 2 2) $ Just 7), (Cell (Point 2 3) $ Just 9), (Cell (Point 2 4) $ Just 8), (Cell (Point 2 5) $ Just 5), (Cell (Point 2 6) $ Just 4), (Cell (Point 2 7) $ Just 3), (Cell (Point 2 8) $ Just 2), (Cell (Point 2 9) $ Just 6),
        (Cell (Point 3 1) $ Just 3), (Cell (Point 3 2) $ Just 8), (Cell (Point 3 3) $ Just 6), (Cell (Point 3 4) $ Just 1), (Cell (Point 3 5) $ Just 7), (Cell (Point 3 6) $ Just 2), (Cell (Point 3 7) $ Just 5), (Cell (Point 3 8) $ Just 4), (Cell (Point 3 9) $ Just 9),
        (Cell (Point 4 1) $ Just 4), (Cell (Point 4 2) $ Just 1), (Cell (Point 4 3) $ Just 2), (Cell (Point 4 4) $ Just 7), (Cell (Point 4 5) $ Just 8), (Cell (Point 4 6) $ Just 3), (Cell (Point 4 7) $ Just 6), (Cell (Point 4 8) $ Just 9), (Cell (Point 4 9) $ Just 5),
        (Cell (Point 5 1) $ Just 9), (Cell (Point 5 2) $ Just 3), (Cell (Point 5 3) $ Just 7), (Cell (Point 5 4) $ Just 2), (Cell (Point 5 5) $ Just 6), (Cell (Point 5 6) $ Just 5), (Cell (Point 5 7) $ Just 4), (Cell (Point 5 8) $ Just 8), (Cell (Point 5 9) $ Just 1),
        (Cell (Point 6 1) $ Just 5), (Cell (Point 6 2) $ Just 6), (Cell (Point 6 3) $ Just 8), (Cell (Point 6 4) $ Just 9), (Cell (Point 6 5) $ Just 4), (Cell (Point 6 6) $ Just 1), (Cell (Point 6 7) $ Just 2), (Cell (Point 6 8) $ Just 3), (Cell (Point 6 9) $ Just 7),
        (Cell (Point 7 1) $ Just 8), (Cell (Point 7 2) $ Just 5), (Cell (Point 7 3) $ Just 4), (Cell (Point 7 4) $ Just 3), (Cell (Point 7 5) $ Just 1), (Cell (Point 7 6) $ Just 7), (Cell (Point 7 7) $ Just 9), (Cell (Point 7 8) $ Just 6), (Cell (Point 7 9) $ Just 2),
        (Cell (Point 8 1) $ Just 6), (Cell (Point 8 2) $ Just 9), (Cell (Point 8 3) $ Just 1), (Cell (Point 8 4) $ Just 4), (Cell (Point 8 5) $ Just 2), (Cell (Point 8 6) $ Just 8), (Cell (Point 8 7) $ Just 7), (Cell (Point 8 8) $ Just 5), (Cell (Point 8 9) $ Just 3),
        (Cell (Point 9 1) $ Just 7), (Cell (Point 9 2) $ Just 2), (Cell (Point 9 3) $ Just 3), (Cell (Point 9 4) $ Just 5), (Cell (Point 9 5) $ Just 9), (Cell (Point 9 6) $ Just 6), (Cell (Point 9 7) $ Just 8), (Cell (Point 9 8) $ Just 1), (Cell (Point 9 9) $ Just 4)
    ]


getEmptyCellsOutput :: [Point]
getEmptyCellsOutput = 
    [
        (Point 1 2), (Point 1 4), (Point 1 6), (Point 1 8), 
        (Point 2 1), (Point 2 2), (Point 2 5), (Point 2 6), (Point 2 7), (Point 2 9), 
        (Point 3 1), (Point 3 3), (Point 3 6), (Point 3 7), (Point 3 8), (Point 3 9), 
        (Point 4 1), (Point 4 4), (Point 4 6), (Point 4 7), (Point 4 9),
        (Point 5 2), (Point 5 4), (Point 5 6), (Point 5 8),
        (Point 6 1), (Point 6 3), (Point 6 4), (Point 6 6), (Point 6 9),
        (Point 7 1), (Point 7 2), (Point 7 3), (Point 7 4), (Point 7 7), (Point 7 9),
        (Point 8 1), (Point 8 3), (Point 8 4), (Point 8 5), (Point 8 8), (Point 8 9),
        (Point 9 2), (Point 9 4), (Point 9 6), (Point 9 8)
    ]

parseGridActual = parse "...........12345...2.6.5.7..58...31..6.....5..17...82..3.7.2.4...21439..........."

parseGridExpected = Right $ 
    Grid 
    [
        (Cell (Point 1 1) Nothing), (Cell (Point 1 2) Nothing), (Cell (Point 1 3) Nothing), (Cell (Point 1 4) Nothing), (Cell (Point 1 5) Nothing), (Cell (Point 1 6) Nothing), (Cell (Point 1 7) Nothing), (Cell (Point 1 8) Nothing), (Cell (Point 1 9) Nothing), 
        (Cell (Point 2 1) Nothing), (Cell (Point 2 2) Nothing), (Cell (Point 2 3) $ Just 1), (Cell (Point 2 4) $ Just 2), (Cell (Point 2 5) $ Just 3), (Cell (Point 2 6) $ Just 4), (Cell (Point 2 7) $ Just 5), (Cell (Point 2 8) Nothing), (Cell (Point 2 9) Nothing), 
        (Cell (Point 3 1) Nothing), (Cell (Point 3 2) $ Just 2), (Cell (Point 3 3) Nothing), (Cell (Point 3 4) $ Just 6), (Cell (Point 3 5) Nothing), (Cell (Point 3 6) $ Just 5), (Cell (Point 3 7) Nothing), (Cell (Point 3 8) $ Just 7), (Cell (Point 3 9) Nothing), 
        (Cell (Point 4 1) Nothing), (Cell (Point 4 2) $ Just 5), (Cell (Point 4 3) $ Just 8), (Cell (Point 4 4) Nothing), (Cell (Point 4 5) Nothing), (Cell (Point 4 6) Nothing), (Cell (Point 4 7) $ Just 3), (Cell (Point 4 8) $ Just 1), (Cell (Point 4 9) Nothing), 
        (Cell (Point 5 1) Nothing), (Cell (Point 5 2) $ Just 6), (Cell (Point 5 3) Nothing), (Cell (Point 5 4) Nothing), (Cell (Point 5 5) Nothing), (Cell (Point 5 6) Nothing), (Cell (Point 5 7) Nothing), (Cell (Point 5 8) $ Just 5), (Cell (Point 5 9) Nothing), 
        (Cell (Point 6 1) Nothing), (Cell (Point 6 2) $ Just 1), (Cell (Point 6 3) $ Just 7), (Cell (Point 6 4) Nothing), (Cell (Point 6 5) Nothing), (Cell (Point 6 6) Nothing), (Cell (Point 6 7) $ Just 8), (Cell (Point 6 8) $ Just 2), (Cell (Point 6 9) Nothing), 
        (Cell (Point 7 1) Nothing), (Cell (Point 7 2) $ Just 3), (Cell (Point 7 3) Nothing), (Cell (Point 7 4) $ Just 7), (Cell (Point 7 5) Nothing), (Cell (Point 7 6) $ Just 2), (Cell (Point 7 7) Nothing), (Cell (Point 7 8) $ Just 4), (Cell (Point 7 9) Nothing), 
        (Cell (Point 8 1) Nothing), (Cell (Point 8 2) Nothing), (Cell (Point 8 3) $ Just 2), (Cell (Point 8 4) $ Just 1), (Cell (Point 8 5) $ Just 4), (Cell (Point 8 6) $ Just 3), (Cell (Point 8 7) $ Just 9), (Cell (Point 8 8) Nothing), (Cell (Point 8 9) Nothing), 
        (Cell (Point 9 1) Nothing), (Cell (Point 9 2) Nothing), (Cell (Point 9 3) Nothing), (Cell (Point 9 4) Nothing), (Cell (Point 9 5) Nothing), (Cell (Point 9 6) Nothing), (Cell (Point 9 7) Nothing), (Cell (Point 9 8) Nothing), (Cell (Point 9 9) Nothing)
    ]


-- test for getEmptyCells
getEmptyCellsTest  = TestCase $ assertEqual "for getEmptyCells," getEmptyCellsOutput (getEmptyCells testGrid)
getEmptyCellsLabel = TestLabel "test getEmptyCells" getEmptyCellsTest

-- test for updateState 
updateStateTest  = TestCase (assertEqual "for updateState," updateGridExpected updateGridActual)
updateStateLabel = TestLabel "test updateState" updateStateTest

-- test for isValidMove
isValidMoveTest  = 
    let validMove1 = isValidMove testGrid (Point 1 2) 4 
        validMove2 = isValidMove testGrid (Point 7 4) 2
        invalidMove1 = not $ isValidMove testGrid (Point 7 4) 6 -- same row
        invalidMove2 = not $ isValidMove testGrid (Point 6 9) 8 -- same column
        invalidMove3 = not $ isValidMove testGrid (Point 8 9) 6 -- same block
    in TestCase (assertBool "for isValidMove," (and [validMove1, validMove2, invalidMove1, invalidMove2, invalidMove3]))
isValidMoveLabel = TestLabel "test isValidMove" isValidMoveTest

-- test for parse
parseTest = TestCase (assertEqual "for parse," parseGridExpected parseGridActual)
parseLabel = TestLabel "test parse" parseTest


-- test for solve
solveTest  = TestCase (assertEqual "for solve," testSolution (solve testGrid (getEmptyCells testGrid) 1))
solveLabel = TestLabel "test solve" solveTest

gridTests = 
    [ 
        getEmptyCellsLabel, 
        updateStateLabel, 
        isValidMoveLabel,
        solveLabel,
        parseLabel
    ]
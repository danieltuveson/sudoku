
import Test.HUnit
import Grid
import Data.Maybe(isNothing)

-- taken from https://www.websudoku.com/?level=1&set_id=141215464
testGrid :: Grid 
testGrid = Grid 
    [
        ((1, 1), Just 2), ((1, 2), Nothing), ((1, 3), Just 5), ((1, 4), Nothing), ((1, 5), Just 3), ((1, 6), Nothing), ((1, 7), Just 1), ((1, 8), Nothing), ((1, 9), Just 8), 
        ((2, 1), Nothing), ((2, 2), Nothing), ((2, 3), Just 9), ((2, 4), Just 8), ((2, 5), Nothing), ((2, 6), Nothing), ((2, 7), Nothing), ((2, 8), Just 2), ((2, 9), Nothing), 
        ((3, 1), Nothing), ((3, 2), Just 8), ((3, 3), Nothing), ((3, 4), Just 1), ((3, 5), Just 7), ((3, 6), Nothing), ((3, 7), Nothing), ((3, 8), Nothing), ((3, 9), Nothing), 
        ((4, 1), Nothing), ((4, 2), Just 1), ((4, 3), Just 2), ((4, 4), Nothing), ((4, 5), Just 8), ((4, 6), Nothing), ((4, 7), Nothing), ((4, 8), Just 9), ((4, 9), Nothing), 
        ((5, 1), Just 9), ((5, 2), Nothing), ((5, 3), Just 7), ((5, 4), Nothing), ((5, 5), Just 6), ((5, 6), Nothing), ((5, 7), Just 4), ((5, 8), Nothing), ((5, 9), Just 1), 
        ((6, 1), Nothing), ((6, 2), Just 6), ((6, 3), Nothing), ((6, 4), Nothing), ((6, 5), Just 4), ((6, 6), Nothing), ((6, 7), Just 2), ((6, 8), Just 3), ((6, 9), Nothing), 
        ((7, 1), Nothing), ((7, 2), Nothing), ((7, 3), Nothing), ((7, 4), Nothing), ((7, 5), Just 1), ((7, 6), Just 7), ((7, 7), Nothing), ((7, 8), Just 6), ((7, 9), Nothing), 
        ((8, 1), Nothing), ((8, 2), Just 9), ((8, 3), Nothing), ((8, 4), Nothing), ((8, 5), Nothing), ((8, 6), Just 8), ((8, 7), Just 7), ((8, 8), Nothing), ((8, 9), Nothing), 
        ((9, 1), Just 7), ((9, 2), Nothing), ((9, 3), Just 3), ((9, 4), Nothing), ((9, 5), Just 9), ((9, 6), Nothing), ((9, 7), Just 8), ((9, 8), Nothing), ((9, 9), Just 4)
    ]

-- what updateGridActual should return
updateGridExpected :: Grid 
updateGridExpected = Grid 
    [
        ((1, 1), Just 9), ((1, 2), Nothing), ((1, 3), Just 5), ((1, 4), Nothing), ((1, 5), Just 3), ((1, 6), Nothing), ((1, 7), Just 1), ((1, 8), Nothing), ((1, 9), Just 8), 
        ((2, 1), Nothing), ((2, 2), Nothing), ((2, 3), Just 9), ((2, 4), Just 8), ((2, 5), Nothing), ((2, 6), Nothing), ((2, 7), Nothing), ((2, 8), Just 2), ((2, 9), Nothing), 
        ((3, 1), Nothing), ((3, 2), Just 8), ((3, 3), Nothing), ((3, 4), Just 5), ((3, 5), Just 7), ((3, 6), Nothing), ((3, 7), Nothing), ((3, 8), Nothing), ((3, 9), Nothing), 
        ((4, 1), Nothing), ((4, 2), Just 1), ((4, 3), Just 2), ((4, 4), Nothing), ((4, 5), Just 8), ((4, 6), Nothing), ((4, 7), Nothing), ((4, 8), Just 9), ((4, 9), Nothing), 
        ((5, 1), Just 9), ((5, 2), Nothing), ((5, 3), Just 7), ((5, 4), Nothing), ((5, 5), Just 6), ((5, 6), Nothing), ((5, 7), Just 4), ((5, 8), Nothing), ((5, 9), Just 1), 
        ((6, 1), Nothing), ((6, 2), Just 6), ((6, 3), Nothing), ((6, 4), Nothing), ((6, 5), Just 4), ((6, 6), Nothing), ((6, 7), Just 2), ((6, 8), Just 3), ((6, 9), Nothing), 
        ((7, 1), Nothing), ((7, 2), Nothing), ((7, 3), Nothing), ((7, 4), Nothing), ((7, 5), Just 1), ((7, 6), Just 7), ((7, 7), Nothing), ((7, 8), Just 6), ((7, 9), Nothing), 
        ((8, 1), Nothing), ((8, 2), Just 9), ((8, 3), Nothing), ((8, 4), Nothing), ((8, 5), Nothing), ((8, 6), Just 8), ((8, 7), Just 7), ((8, 8), Just 2), ((8, 9), Nothing), 
        ((9, 1), Just 7), ((9, 2), Nothing), ((9, 3), Just 3), ((9, 4), Nothing), ((9, 5), Just 9), ((9, 6), Nothing), ((9, 7), Just 8), ((9, 8), Nothing), ((9, 9), Just 5)
    ]

updateGridActual :: Grid
updateGridActual = 
    let ins1 = updateState testGrid (3, 4) 5
        ins2 = updateState ins1 (1,1) 9
        ins3 = updateState ins2 (9,9) 5
        ins4 = updateState ins3 (8,8) 2
    in ins4



-- full solution to testGrid
testSolution :: Maybe Grid 
testSolution = Just $ Grid 
    [
        ((1, 1), Just 2), ((1, 2), Just 4), ((1, 3), Just 5), ((1, 4), Just 6), ((1, 5), Just 3), ((1, 6), Just 9), ((1, 7), Just 1), ((1, 8), Just 7), ((1, 9), Just 8),
        ((2, 1), Just 1), ((2, 2), Just 7), ((2, 3), Just 9), ((2, 4), Just 8), ((2, 5), Just 5), ((2, 6), Just 4), ((2, 7), Just 3), ((2, 8), Just 2), ((2, 9), Just 6),
        ((3, 1), Just 3), ((3, 2), Just 8), ((3, 3), Just 6), ((3, 4), Just 1), ((3, 5), Just 7), ((3, 6), Just 2), ((3, 7), Just 5), ((3, 8), Just 4), ((3, 9), Just 9),
        ((4, 1), Just 4), ((4, 2), Just 1), ((4, 3), Just 2), ((4, 4), Just 7), ((4, 5), Just 8), ((4, 6), Just 3), ((4, 7), Just 6), ((4, 8), Just 9), ((4, 9), Just 5),
        ((5, 1), Just 9), ((5, 2), Just 3), ((5, 3), Just 7), ((5, 4), Just 2), ((5, 5), Just 6), ((5, 6), Just 5), ((5, 7), Just 4), ((5, 8), Just 8), ((5, 9), Just 1),
        ((6, 1), Just 5), ((6, 2), Just 6), ((6, 3), Just 8), ((6, 4), Just 9), ((6, 5), Just 4), ((6, 6), Just 1), ((6, 7), Just 2), ((6, 8), Just 3), ((6, 9), Just 7),
        ((7, 1), Just 8), ((7, 2), Just 5), ((7, 3), Just 4), ((7, 4), Just 3), ((7, 5), Just 1), ((7, 6), Just 7), ((7, 7), Just 9), ((7, 8), Just 6), ((7, 9), Just 2),
        ((8, 1), Just 6), ((8, 2), Just 9), ((8, 3), Just 1), ((8, 4), Just 4), ((8, 5), Just 2), ((8, 6), Just 8), ((8, 7), Just 7), ((8, 8), Just 5), ((8, 9), Just 3),
        ((9, 1), Just 7), ((9, 2), Just 2), ((9, 3), Just 3), ((9, 4), Just 5), ((9, 5), Just 9), ((9, 6), Just 6), ((9, 7), Just 8), ((9, 8), Just 1), ((9, 9), Just 4)
    ]


getEmptyCellsOutPut :: Points
getEmptyCellsOutPut = 
    [
        (1, 2), (1, 4), (1, 6), (1, 8), 
        (2, 1), (2, 2), (2, 5), (2, 6), (2, 7), (2, 9), 
        (3, 1), (3, 3), (3, 6), (3, 7), (3, 8), (3, 9), 
        (4, 1), (4, 4), (4, 6), (4, 7), (4, 9),
        (5, 2), (5, 4), (5, 6), (5, 8),
        (6, 1), (6, 3), (6, 4), (6, 6), (6, 9),
        (7, 1), (7, 2), (7, 3), (7, 4), (7, 7), (7, 9),
        (8, 1), (8, 3), (8, 4), (8, 5), (8, 8), (8, 9),
        (9, 2), (9, 4), (9, 6), (9, 8)
    ]


-- test for getEmptyCells
getEmptyCellsTest  = TestCase $ assertEqual "for getEmptyCells," getEmptyCellsOutPut (getEmptyCells testGrid)
getEmptyCellsLabel = TestLabel "test getEmptyCells" getEmptyCellsTest

-- test for updateState 
updateStateTest  = TestCase (assertEqual "for updateState," updateGridExpected updateGridActual)
updateStateLabel = TestLabel "test updateState" updateStateTest

-- test for isValidMove
isValidMoveTest  = 
    let validMove1 = isValidMove testGrid (1, 2) 4 
        validMove2 = isValidMove testGrid (7, 4) 2
        invalidMove1 = not $ isValidMove testGrid (7, 4) 6 -- same row
        invalidMove2 = not $ isValidMove testGrid (6, 9) 8 -- same column
        invalidMove3 = not $ isValidMove testGrid (8, 9) 6 -- same block
    in TestCase (assertBool "for isValidMove," (and [validMove1, validMove2, invalidMove1, invalidMove2, invalidMove3]))
isValidMoveLabel = TestLabel "test isValidMove" isValidMoveTest

-- test for solve
solveTest  = TestCase (assertEqual "for solve," testSolution (solve testGrid (getEmptyCells testGrid) 1))
solveLabel = TestLabel "test solve" solveTest

-- test2 = TestCase (do (x,y) <- partA 3
--                      assertEqual "for the first result of partA," 5 x
--                      b <- partB y
--                      assertBool ("(partB " ++ show y ++ ") failed") b)

tests = 
    TestList 
        [
            getEmptyCellsLabel, 
            updateStateLabel, 
            isValidMoveLabel,
            solveLabel
        ]

main :: IO ()
main = 
    do 
        results <- runTestTT tests
        print results

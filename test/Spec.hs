import Test.HUnit
import Grid
import Data.Maybe(isNothing)

-- taken from https://www.websudoku.com/?level=1&set_id=141215464
testGrid :: [((RowNum, ColNum), Maybe Int)] 
testGrid = 
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

testEmptyCells :: [(RowNum, ColNum)]
testEmptyCells = map fst (filter (\x -> isNothing $ snd x) testGrid)

-- inputState :: [((RowNum, ColNum), Maybe Int)] 
-- inputState = updateState state point num 5

-- expectedOutputState :: [((RowNum, ColNum), Maybe Int)] 


test1 = TestCase (assertEqual "for testGrid," testGrid [((1, 2), Just 5)])

-- test2 = TestCase (do (x,y) <- partA 3
--                      assertEqual "for the first result of partA," 5 x
--                      b <- partB y
--                      assertBool ("(partB " ++ show y ++ ") failed") b)

tests = TestList [TestLabel "test1" test1] --, TestLabel "test2" test2]

main :: IO ()
main = 
    do 
        results <- runTestTT tests
        print results

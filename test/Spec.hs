import Test.HUnit
import Grid
import GridTest

tests = TestList gridTests

main :: IO ()
main = 
    do 
        results <- runTestTT tests
        print results

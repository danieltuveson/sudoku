module Lib(runSudoku) where

import Parse
import Solve

runSudoku :: IO ()
runSudoku = 
    do 
        line <- getLine
        case parse line of 
            Left err   -> print err
            Right grid -> 
                case getSolution grid of 
                    Nothing       -> print "invalid sudoku, no solution exists."
                    Just solution -> print $ "sudoku solved!\n" ++ show solution

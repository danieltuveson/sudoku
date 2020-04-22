module Main where

import Sudoku(solveLine)

main :: IO ()
main = getLine >>= (return . solveLine) >>= print 

module Main where

import Lib(solveLine)

main :: IO ()
main = getLine >>= (return . solveLine) >>= print 

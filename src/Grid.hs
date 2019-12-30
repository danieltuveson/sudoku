module Grid where 

import Data.Array

{- 
    - Used to describe the structure of a sudoku cell 
    - Each cell will exist in a row, column and block
    - Rows will be numbered 1-9 left top to bottom
    - Columns will be similarly numbered 1-9 left to right
    - Blocks will be similarly numbered 1-9 left to right, top to bottom
    - isInitial is true if the cell was created by the constructor, false otherwise
-}
data Cell = Cell {
    contents :: Maybe Int, 
    isInitial :: Bool,
    block :: Int
} deriving(Show)

type Row = Int
type Col = Int 

-- Defines the game grid used for sudoku
data Grid = Grid [[Cell]]
    deriving(Show)

-- Takes a list of rows that wil define the inital state of our grid.
-- This assumes the input is well formed, i.e. a list of 9 lists of
-- ints between 1-9. 
gridInit :: [[Int]] -> Grid 
gridInit ls = Grid $ map initRow ls 

initRow :: [Int] -> [Cell]
initRow _ = [Cell { contents = Just 5, isInitial = True, block = 5 }]
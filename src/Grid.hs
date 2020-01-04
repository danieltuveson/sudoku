module Grid where 

import Data.Array
import Data.List.Index(indexed)
import Data.Maybe(isJust)


type RowNum    = Int
type ColNum    = Int 
type Point     = (RowNum, ColNum)
-- type BlockNum  = Int 
type CellValue = Maybe Int 

{- 
    - Used to describe the structure of a sudoku cell 
    - Each cell will exist in a row, column and block
    - Rows will be numbered 1-9 left top to bottom
    - Columns will be similarly numbered 1-9 left to right
    - Blocks will be similarly numbered 1-9 left to right, top to bottom
    - isInitial is true if the cell was created by the constructor, false otherwise
-}
data Cell = Cell (Point, CellValue) deriving(Show)



-- Defines the game grid used for sudoku
type Grid = [Cell]

instance Show Grid where 
    show cell@(Cell ((_, ColNum), _))
        | 

-- -- Takes a list of rows that wil define the inital state of our grid.
-- -- This assumes the input is well formed, i.e. a list of 9 lists of
-- -- ints between 1-9, with both rows and columns being indexed 0-9
-- initGrid :: [(RowNum, [(ColNum, Maybe Int)])] -> Grid
-- initGrid grid = Grid $ map initRow grid
--     where initRow (i, row) = map (initCell i) row

-- -- Initializes a given cell in our grid
-- initCell :: RowNum -> (ColNum, Maybe Int) -> Cell
-- initCell i (j, maybeNum) = 
--     Cell {
--         contents  = maybeNum, 
--         isInitial = True, 
--         block     = getBlock i j
--     }

-- Returns the block associated with a given cell's row & column
getBlock :: RowNum -> ColNum -> BlockNum
getBlock i j = 3 * (getSeg i - 1) + getSeg j 
    where getSeg num
            | elem num [1, 2, 3] = 1 
            | elem num [4, 5, 6] = 2
            | elem num [7, 8, 9] = 3

-- getSeg :: ColNum -> Int
-- getSeg num
--     | elem num [1, 2, 3] = 1 
--     | elem num [4, 5, 6] = 2
--     | elem num [7, 8, 9] = 3

data Seg = One | Two | Three 
-- getBlock _ _ = error "this should really not happen lol"


-- takes in state of the board as a list of (Row, Col, Maybe Value) 
-- and a list of (Row, Col, Maybe Value) (with Maybe value being Nothing) to fill
-- and returns the solution if it is solvable 
solve :: Grid -> [(RowNum, ColNum)] -> Int -> Maybe Grid
solve _ _ num | num > 9      = Nothing 
solve state [] _             = Just []
solve state ls@(x : xs) num
        | isValidMove state x num 
          && isJust solution = solution 
        | otherwise          = solve state ls (num + 1)
    where solution = solve (updateState state x num) xs num
-- solve state (x : ls) num = takeWhile fun [1..9]


isValidMove :: Grid -> (RowNum, ColNum) -> Int -> Bool
isValidMove ls _ _ = True

updateState :: Grid -> (RowNum, ColNum) -> Int -> Grid
updateState state point num = map (replace point num) state 
    where replace point num cell@(sPoint, _)
            | sPoint == point = (point, Just num)
            | otherwise       = cell




module Grid where 

-- import qualified Data.Map.Strict as Map
import Data.Maybe(isJust, isNothing)


type RowNum    = Int
type ColNum    = Int 
type BlockNum  = Int 
type CellValue = Maybe Int 
data Seg       = One | Two | Three 

type Point     = (RowNum, ColNum)          -- defines the location of a cell in a sudoku grid
type Points    = [(RowNum, ColNum)]
type Cell      = (Point, CellValue)        -- defines the cells of the grid used for sudoku
data Grid      = Grid [Cell] deriving(Eq)  -- defines the grid used for sudoku

instance Show Grid where 
    show (Grid []) = ""
    show (Grid (((_, colNum), maybeVal) : cells)) = 
        let delimeter = if colNum == 9 then "\n" else " " 
            val       = case maybeVal of 
                            Nothing -> "_"
                            Just x  -> show x 
        in val ++ delimeter ++ show (Grid cells)


-- Returns the block associated with a given cell's row & column
getBlock :: RowNum -> ColNum -> BlockNum
getBlock i j = 3 * (getSeg i - 1) + getSeg j 
    where getSeg num
            | elem num [1, 2, 3] = 1 
            | elem num [4, 5, 6] = 2
            | elem num [7, 8, 9] = 3



-- takes in state of the board as a list of (Row, Col, Maybe Value) 
-- and a list of (Row, Col, Maybe Value) (with Maybe value being Nothing) to fill
-- and returns the solution if it is solvable 
solve :: Grid -> Points -> Int -> Maybe Grid
solve _ _ num | num > 9      = Nothing 
solve state [] _             = Just $ Grid []
solve state ls@(x : xs) num
        | isValidMove state x num 
          && isJust solution = solution 
        | otherwise          = solve state ls (num + 1)
    where solution = solve (updateState state x num) xs 1


isValidMove :: Grid -> Point -> Int -> Bool
isValidMove ls _ _ = True


updateState :: Grid -> Point -> Int -> Grid
updateState (Grid state) point num = Grid $ map (replace point num) state 
    where replace point num cell@(sPoint, _)
            | sPoint == point = (point, Just num)
            | otherwise       = cell


-- Returns a list containing all the points with empty cells in grid
getEmptyCells :: Grid -> Points
getEmptyCells (Grid grid) = map fst (filter (\x -> isNothing $ snd x) grid)

module Grid where 

-- import qualified Data.Map.Strict as Map
import Data.Maybe(isNothing)

type RowNum    = Int
type ColNum    = Int 
type BlockNum  = Int 
type CellValue = Maybe Int 
data Seg       = One | Two | Three 

type Point     = (RowNum, ColNum)          -- defines the location of a cell in a sudoku grid
type Points    = [(RowNum, ColNum)]
type Cell      = (Point, CellValue)        -- defines the cells of the grid used for sudoku
type Cells     = [Cell]
data Grid      = Grid [Cell] deriving(Eq)  -- defines the grid used for sudoku

instance Show Grid where 
    show g = "\n" ++ showGrid g

-- override default print
showGrid :: Grid -> String 
showGrid (Grid []) = ""
showGrid (Grid (((_, colNum), maybeVal) : cells)) = 
    let delimeter = if colNum == 9 then "\n" else " " 
        val       = case maybeVal of 
                        Nothing -> "_"
                        Just x  -> show x 
    in val ++ delimeter ++ showGrid (Grid cells)


-- Returns a list containing all the points with empty cells in grid
getEmptyCells :: Grid -> Points
getEmptyCells (Grid grid) = map fst (filter (\x -> isNothing $ snd x) grid) 

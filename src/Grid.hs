module Grid where 

-- import qualified Data.Map.Strict as Map
import Data.Maybe(isNothing)

-- defines the location of a cell in a sudoku grid
data Point = Point 
    { rowNum :: Int
    , colNum :: Int
    } deriving(Show, Eq, Ord)

-- defines the cells of the grid used for sudoku
data Cell = Cell 
    { cellPoint :: Point
    , value :: Maybe Int
    } deriving(Show, Eq, Ord)

-- defines the grid used for sudoku
newtype Grid = Grid [Cell] 
    deriving(Show, Eq, Ord)

-- override default print
showGrid :: Grid -> String 
showGrid (Grid cells) = "\n" ++ foldr (\cell rest -> val cell ++ delimeter cell ++ rest) "" cells
    where 
        delimeter cell = if (colNum $ cellPoint cell) == 9 then "\n" else " " 
        val cell = 
            case value cell of 
                Nothing -> "_"
                Just x  -> show x 

-- Returns a list containing all the points with empty cells in grid
getEmptyCells :: Grid -> [Point]
getEmptyCells (Grid grid) = cellPoint <$> filter (isNothing . value) grid

module Solve where 

import Data.Maybe(isJust)

import Grid
import Validation(isValidMove)

-- returns the solution to an unsolved sudoku, if it exists 
getSolution :: Grid -> Maybe Grid
getSolution g = solve g (getEmptyCells g) 1


-- takes in state of the board as a list of (Row, Col, Maybe Value) 
-- and a list of (Row, Col, Maybe Value) (with Maybe value being Nothing) to fill
-- and returns the solution if it is solvable 
solve :: Grid -> Points -> Int -> Maybe Grid
solve _ _ num | num > 9      = Nothing 
solve state [] _             = Just state
solve state ls@(x : xs) num
        | isValidMove state x num 
          && isJust solution = solution 
        | otherwise          = solve state ls (num + 1)
    where solution = solve (updateState state x num) xs 1


-- Adds (point, Just num) to the grid
updateState :: Grid -> Point -> Int -> Grid
updateState (Grid state) point num = Grid $ map (replace point num) state 
    where replace point num cell@(sPoint, _)
            | sPoint == point = (point, Just num)
            | otherwise       = cell
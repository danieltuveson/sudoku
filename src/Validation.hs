module Validation(isValidMove) where 

import Grid

isValidMove :: Grid -> Point -> Int -> Bool
isValidMove g p i = (isValidRow g p i) && (isValidCol g p i) && (isValidBlock g p i) 


isValidRow :: Grid -> Point -> Int -> Bool
isValidRow (Grid cells) (rowNum, _) num = 
    let row = filter (\cell -> fst (fst cell) == rowNum) cells in -- get row with rowNum
        not $ containsAny row num 


isValidCol :: Grid -> Point -> Int -> Bool
isValidCol (Grid cells) (_, colNum) num = 
    let col = filter (\cell -> snd (fst cell) == colNum) cells in -- get col with colNum
        not $ containsAny col num 


isValidBlock :: Grid -> Point -> Int -> Bool
isValidBlock (Grid cells) point num = 
    let blockNum = uncurry getBlock point
        block    = filter (\(gPoint, _) -> uncurry getBlock gPoint == blockNum) cells -- get block with blockNum
    in not $ containsAny block num 


-- Returns the block number associated with a given cell's row & column.
-- Each block in the grid is associated with a number 1-9, as read left to 
-- right, top to bottom. e.g.
-- 1  2  3 
-- 4  5  6
-- 7  8  9
getBlock :: RowNum -> ColNum -> BlockNum
getBlock i j = 3 * (getSeg i - 1) + getSeg j 
    where getSeg num
            | elem num [1, 2, 3] = 1 
            | elem num [4, 5, 6] = 2
            | elem num [7, 8, 9] = 3

-- sees if any cells have a CellValue of num
containsAny :: Cells -> Int -> Bool 
containsAny cells num = elem (Just num) (map snd cells)
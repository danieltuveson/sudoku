module Parse where 

import Grid
import Data.Char(isDigit, digitToInt)

type Error = String 

-- takes in a string
parse :: String -> Either Error Grid
parse str
    | length str /= 81 = Left $ "Expected 81 characters, but only got " ++ (show $ length str) ++ " characters. Please try again"
    | otherwise        = 
        case span isValidChar str of 
            (g, [])       -> Right $ toGrid g
            (_, (x : xs)) -> Left $ "error parsing '" ++ [x] ++ "', invalid character"
    where isValidChar char = isDigit char || char == '.'


-- takes the form of a string .11..3.9...3 etc and converts it to a 
-- grid if possible. Returns nothing if the string is invalid
-- assumes input is 81 characters and string only contains 1 - 9 and '.'
toGrid :: String -> Grid 
toGrid str = Grid $ zip points (toValues str)
    where points  = [(i,j) | i <- [1..9], j <- [1..9]]

toValues :: String -> [CellValue]
toValues str = map toValue str
    where toValue ch
                | ch == '.'  = Nothing 
                | otherwise  = Just $ digitToInt ch 
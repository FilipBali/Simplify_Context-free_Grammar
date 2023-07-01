{-
Author: Filip Bali
Date: 2022
Module: InputHandler
Describtion: Contains functions to handle program input.
-}

module InputHandler (parseArgs,
                     loadInput, 
                     iterItems
                     ) where

import Data.List.Split


----------------------------
-- handle program arguments
----------------------------
parseArgs :: [String] -> (String,[String])
parseArgs [] = error "Error: no parameter"
parseArgs (_x:_y:_z:_xs) = error "Error: too many parameters" 
parseArgs [x] = parseArgs [x, ""]
parseArgs (x:xs)
    | x == "-1" = ("-1", xs)
    | x == "-2" = ("-2", xs)
    | x == "-i" = ("-i", xs)
    | otherwise = error "Error: unknown parameter" 


-----------------
-- Split by ","
-----------------
parseStr :: String -> [String]
parseStr = splitOn ","


-------------------------------------------------
-- Parse input data and save it as nested array
-------------------------------------------------
iterItems :: [String] -> [[String]]
iterItems = map parseStr


------------------------------------------------------
-- if filename is not set then load input from stdin
-- if filename is set then load input from that file
------------------------------------------------------
loadInput :: String -> IO [String]
loadInput x
    | x == "" = splitOn "\n" `fmap` getContents
    | otherwise = splitOn "\n" `fmap`readFile x

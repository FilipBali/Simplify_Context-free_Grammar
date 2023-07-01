{-
Author: Filip Bali
Date: 2022
Module: Parse
Describtion: Contains functions to parse string/list.
-}

module Parse (stringToCharsList,
              parseRule,
              toCommaSeparatedString,
              deletetLastIfEmpty
              ) where

import Data.List.Split
import Data.List


-------------------------------------
-- parse string into array of chars
-------------------------------------
stringToCharsList :: String -> [String]
stringToCharsList = map (:[])


----------------------------------------------------------------
-- split rule by "->" symbol
-- returns array with two items => left and right side of rule
----------------------------------------------------------------
parseRule :: String -> [String]
parseRule = splitOn "->"


-----------------------------------------------
-- creates string of chars separated by commas
-----------------------------------------------
toCommaSeparatedString :: [String] -> String
toCommaSeparatedString = intercalate ","


-----------------------------------------
-- Deletes empty ("") strings from array
-----------------------------------------
deletetLastIfEmpty :: [String] -> [String]
deletetLastIfEmpty xs = [ x | x <- xs , not (null x) ]

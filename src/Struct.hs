{-
Author: Filip Bali
Date: 2022
Module: Struct
Describtion: Contains used structures.
-}

module Struct (BKGStruct(..)) where


----------------------------------------
-- Represents BKG as program strucutre
----------------------------------------
data BKGStruct = BKGStruct { nonTerminals :: [String]
                           , terminals :: [String]
                           , startSymbol :: String
                           , rules :: [String]
                           } deriving (Show)

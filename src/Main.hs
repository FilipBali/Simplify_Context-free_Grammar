{-
Author: Filip Bali
Date: 2022
Module: Main
Describtion: Main function of program. Contains main function and other functions to identify the tasks.
-}

module Main (main) where

import InputHandler
import Parse
import Struct
import SimplificationCore
import SimplificationLogic
import Validation

import System.Environment   

main :: IO ()
main = do

    -- Load program params
    args <- getArgs
    
    ----------------------------------------
    -- Parse program input from stdin/file
    ----------------------------------------
    let programControlParam = parseArgs args
    file_data_arr <- loadInput (head (snd programControlParam))
    let data_collection = iterItems file_data_arr

    let nonTerminalsArr = head data_collection
    let terminalsArr = data_collection !! 1
    let startSymbolArr = data_collection !! 2
    let startSymbolStr = concat startSymbolArr
    let rulesNestedArr = splitAt 3 data_collection
    let rulesArr = deletetLastIfEmpty (concat (snd rulesNestedArr))    


    ------------------------
    -- Print program output
    ------------------------
    printArray (concat (taskChooser (fst programControlParam) 
                                    nonTerminalsArr
                                    terminalsArr
                                    startSymbolStr
                                    rulesArr
                        ))


-----------------------------------------------------------
-- Determine program task according to program parameters.
-----------------------------------------------------------
taskChooser :: String -> [String] -> [String] -> String -> [String] -> [[String]]
taskChooser programControlParam nonTerminalsArr terminalsArr startSymbolStr rulesArr
    | programControlParam == "-1" && bkgValidation bkg = grammarSimplification1Output bkg
    | programControlParam == "-2" && bkgValidation bkg = grammarSimplification2Output bkg bkg2
    | programControlParam == "-i" && bkgValidation bkg = showGrammar bkg
    | otherwise = error "Error: unknown parameter or invalid grammar" 
    where bkg = BKGStruct {nonTerminals = nonTerminalsArr,
                       terminals = terminalsArr,
                       startSymbol = startSymbolStr,
                       rules = rulesArr
                      }

          bkg2 = BKGStruct {nonTerminals = nonTerminalsArr,
                                           terminals = terminalsArr,
                                           startSymbol = startSymbolStr,
                                           rules = grammarSimplification1 (getTermRulesOnly (rules bkg)) bkg
                                           }


--------------------------------------------------------
-- prints grammar from program structure (-i parameter)
--------------------------------------------------------
showGrammar :: BKGStruct -> [[String]]
showGrammar bkg
    = [[toCommaSeparatedString( nonTerminals bkg)],
       [toCommaSeparatedString( terminals bkg)],
       [startSymbol bkg],
       rules bkg
      ]

--------------------------
-- prints array on stdout
-- every item on newline
--------------------------
printArray :: [String] -> IO ()
printArray = mapM_ putStrLn
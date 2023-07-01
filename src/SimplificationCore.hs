{-
Author: Filip Bali
Date: 2022
Module: SimplificationCore
Describtion: Contains functions which are abstraction layer between logic module and rest of the program.
-}

module SimplificationCore (grammarSimplification1Output,
                           grammarSimplification2Output,
                           grammarSimplification1,
                           grammarSimplification2
                           ) where

import Struct
import SimplificationLogic
import Validation
import Parse
import Data.List

-----------------------------------------------------------------------------------------
-- Returns new grammar in list form after first step simplification 
-----------------------------------------------------------------------------------------
grammarSimplification1Output :: BKGStruct -> [[String]]
grammarSimplification1Output bkg
    = [[toCommaSeparatedString (isStartSymbolInNonterminalArrayBool isStartSymbInTermArrBoolResult newNonterminals (startSymbol bkg))],
       [toCommaSeparatedString (removeDuplicates (terminals bkg))],
       [startSymbol bkg],
       grammarSimplification1 (getTermRulesOnly newRules) bkg
      ]
    where       newRules = grammarSimplification1 (getTermRulesOnly (rules bkg)) bkg
                newNonterminals = getLeftRuleNonterminalArr newRules
                isStartSymbInTermArrBoolResult = checkIfContains (getLeftRuleNonterminalArr newRules) [] [startSymbol bkg]


-----------------------------------------------------------------------------------------
-- Returns new grammar in list form after second step simplification
-----------------------------------------------------------------------------------------
grammarSimplification2Output :: BKGStruct -> BKGStruct ->[[String]]
grammarSimplification2Output bkg bkg2
    = [[toCommaSeparatedString (isStartSymbolInNonterminalArrayBool isStartSymbInTermArrBoolResult newNonterminals (startSymbol bkg))],
       [toCommaSeparatedString (removeDuplicates (filterTerminalOnly (stringToCharsList (intercalate "" (getRightRuleNonterminalArr newRules)))))],
       [startSymbol bkg2],
       grammarSimplification2 (startSymbol bkg : ["#"]) bkg2
      ]
    where       newRules = grammarSimplification2 (startSymbol bkg : ["#"]) bkg2
                newNonterminals = getLeftRuleNonterminalArr newRules
                isStartSymbInTermArrBoolResult = checkIfContains (getLeftRuleNonterminalArr newRules) [] [startSymbol bkg]



-----------------------------------------------------------------------------------------
-- Returns new rules after first step simplification
-----------------------------------------------------------------------------------------
grammarSimplification1 :: [String] -> BKGStruct -> [String]
grammarSimplification1 termRulesOnly bkg
    | termRulesOnlyTmp == termRulesOnlyNew = ifRulesAreOnlyHastag (ifHastagOnlyArr (getRightRuleNonterminalArr termRulesOnlyTmp)) termRulesOnlyTmp
    | otherwise = grammarSimplification1 termRulesOnlyNew bkg
     where 
         termRulesOnlyTmp = termRulesOnly
         termRulesOnlyNew = removeDuplicates(getRulesArr termRulesOnly (rules bkg))


-----------------------------------------------------------------------------------------
-- Returns new grammar after second step simplification
-----------------------------------------------------------------------------------------
grammarSimplification2 :: [String] -> BKGStruct -> [String]
grammarSimplification2 startArr bkg
    | startArr == startArrNew = getSecondStepFinalRules startArrNew (rules bkg)
    | otherwise = grammarSimplification2 startArrNew bkg
     where
         startArrNew = removeDuplicates (deleteUselessRules startArr (rules bkg))

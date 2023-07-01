{-
Author: Filip Bali
Date: 2022
Module: SimplificationLogic
Describtion: Contains functions which are responsible for creating new grammar.
-}


module SimplificationLogic (deleteUselessRules,
                            ruleCandidateSolverSPA,
                            isStartSymbolInNonterminalArrayBool,
                            addToArrBool,
                            getSecondStepFinalRules,
                            getTermRulesOnly,
                            removeDuplicates,
                            privateLeftRuleNonterminalArr,
                            getLeftRuleNonterminalArr,
                            getRightRuleNonterminalArr,
                            checkIfContains,
                            deleteLowerCaseFromList,
                            ruleCandidateSolverFPA,
                            getRulesArr,
                            filterNonterminalOnly,
                            filterTerminalOnly,
                            ) where

import Parse
import Validation

import Data.Set (toList, fromList)


--------------------------------------------------------------
-- Remove rules which are not suitable for new grammar
-- First paramerter [String] = Aleready accepter nonterminals
-- Second parameter [String] = Array of rules
--------------------------------------------------------------
deleteUselessRules :: [String] -> [String] -> [String]
deleteUselessRules _ [] = []
deleteUselessRules acceptedNontermArr [ruleX] = addToArrBool (ruleCandidateSolverSPA acceptedNontermArr [ruleX]) (filterNonterminalOnly (stringToCharsList (head (getRightRuleNonterminalArr [ruleX])))) ++ acceptedNontermArr
deleteUselessRules acceptedNontermArr (ruleX:ruleXS) = addToArrBool (ruleCandidateSolverSPA acceptedNontermArr [ruleX]) (filterNonterminalOnly (stringToCharsList (head (getRightRuleNonterminalArr [ruleX])))) ++ deleteUselessRules acceptedNontermArr ruleXS


---------------------------------------------------------------------
-- First parameter [String] = Array of already accepted nonterminals
-- Second parameter [String] = Candidating rule
-- Assess whether the rule can be accepted for the output grammar
-- SPA = Second part of algorithm (= -2 parameter)
---------------------------------------------------------------------
ruleCandidateSolverSPA :: [String] -> [String] -> Bool
ruleCandidateSolverSPA acceptedNontermArr ruleCandidate = checkIfContains acceptedNontermArr [] (getLeftRuleNonterminalArr ruleCandidate)


--

--------------------------------------------
-- Checks if start symbol is in nonterminal 
--------------------------------------------
isStartSymbolInNonterminalArrayBool :: Bool -> [String] -> String -> [String]
isStartSymbolInNonterminalArrayBool boolValue nontermArr startSymb
    | boolValue = nontermArr
    | otherwise = nontermArr ++ [startSymb]


--------------------------------------------------------------------------------
-- ***supportive function*** - approves if array can be added into final result
-- If bool is True, returns param array, otherwise returns empty array
--------------------------------------------------------------------------------
addToArrBool :: Bool -> [String] -> [String]
addToArrBool boolValue arr
    | boolValue = arr
    | otherwise = []


-----------------------------------------------------------------------------------------
-- Returns array of rules which are accepted for new grammar in second part of algorithm
-----------------------------------------------------------------------------------------
getSecondStepFinalRules :: [String] -> [String] -> [String]
getSecondStepFinalRules _ [] = []
getSecondStepFinalRules accpts [x] = addToArrBool (checkIfContains accpts [] (getLeftRuleNonterminalArr [x])) [x]
getSecondStepFinalRules accpts (x:xs) = addToArrBool (checkIfContains accpts [] (getLeftRuleNonterminalArr [x])) [x] ++ getSecondStepFinalRules accpts xs
    

-----------------------------------------------------------------
-- Returns array of rules which has on right side only terminals
-----------------------------------------------------------------
getTermRulesOnly :: [String] -> [String]
getTermRulesOnly [] = []
getTermRulesOnly [x] = addToArrBool (isTerminalsArr (tail (parseRule x))) [x]
getTermRulesOnly (x:xs) = addToArrBool (isTerminalsArr (tail (parseRule x))) [x] ++ getTermRulesOnly xs

--

--------------------------------------
-- Remove duplicates in array
-- Returns array with unique elements
--------------------------------------
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList


-------------------------------------------------
-- START BLOCK: PSEUDO PRIVATE FUNCTION
-- 
-- PRIVATE SCOPE FOR: getLeftRuleNonterminalArr
-- 
-- ***suportive function***
-- Extrats element from array structure
------------------------------------------------
privateLeftRuleNonterminalArr :: [String] -> [String]
privateLeftRuleNonterminalArr [] = []
privateLeftRuleNonterminalArr [x] =  [head (parseRule x)]
privateLeftRuleNonterminalArr (x:xs) = head (parseRule x) : privateLeftRuleNonterminalArr xs
-- ------------------------------------
-- END BLOCK: PSEUDO PRIVATE FUNCTION
-- ------------------------------------


----------------------------------------
-- Returns array of left sides of rules
----------------------------------------
getLeftRuleNonterminalArr :: [String] -> [String]
getLeftRuleNonterminalArr [] = []
getLeftRuleNonterminalArr x = removeDuplicates (privateLeftRuleNonterminalArr x)


-----------------------------------------
-- Returns array of right sides of rules
-----------------------------------------
getRightRuleNonterminalArr :: [String] -> [String]
getRightRuleNonterminalArr [] = []
getRightRuleNonterminalArr [x] =  tail (parseRule x)
getRightRuleNonterminalArr (x:xs) = tail (parseRule x) ++ getRightRuleNonterminalArr xs


-------------------------------------------------------
-- Description: Identify and get nonterminals
-- Deletes lower case symbols from array
-- Returns empty or array with upper case symbols only
-------------------------------------------------------
deleteLowerCaseFromList :: [String] -> [String]
deleteLowerCaseFromList [] = []
deleteLowerCaseFromList [x] = filter isNonterminals [x]
deleteLowerCaseFromList (x:xs) = filter isNonterminals [x] ++ deleteLowerCaseFromList xs


------------------------------------------------------------------
-- First parameter [String] = Array of already accepted rules
-- Second parameter [String] = Candidating rule
-- Assess whether the rule can be accepted for the output grammar
-- FPA = First part of algorithm (= -1 parameter)
------------------------------------------------------------------
ruleCandidateSolverFPA :: [String] -> [String] -> Bool
ruleCandidateSolverFPA acceptedRules ruleCandidate = checkIfContains (getLeftRuleNonterminalArr acceptedRules) [] (deleteLowerCaseFromList (stringToCharsList (head (getRightRuleNonterminalArr ruleCandidate))))


------------------------------------------------------------------------------------
-- Iterate over all rules
-- Returns all rules which satifies conditions in subfunctions/supportive functions
------------------------------------------------------------------------------------
getRulesArr :: [String] -> [String] -> [String]
getRulesArr _ [] = []
getRulesArr acceptedRules [ruleX] = addToArrBool (ruleCandidateSolverFPA acceptedRules [ruleX]) [ruleX] ++ acceptedRules
getRulesArr acceptedRules (ruleX:ruleXS) = addToArrBool (ruleCandidateSolverFPA acceptedRules [ruleX]) [ruleX] ++ getRulesArr acceptedRules ruleXS


-------------------------------------------------------------------
-- returns array of items which are upper case only (nonterminals)
-------------------------------------------------------------------
filterNonterminalOnly :: [String] -> [String]
filterNonterminalOnly xs = [x | x <- xs, isNonterminal x]


-------------------------------------------------------------------
-- returns array of items which are lower case only (nonterminals)
-------------------------------------------------------------------
filterTerminalOnly :: [String] -> [String]
filterTerminalOnly xs = [x | x <- xs, isTerminal x]
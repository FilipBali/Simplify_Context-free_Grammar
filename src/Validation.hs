{-
Author: Filip Bali
Date: 2022
Module: Validation
Describtion: Contains functions to validate content.
-}


module Validation (bkgValidation,
                   checkRules,
                   getIsLowerOrHashtagBool,
                   isLowerOrHashtag,
                   isHashtag,
                   ifHastagOnly,
                   ifHastagOnlyArr,
                   ifRulesAreOnlyHastag,
                   isTerminal,
                   isTerminals,
                   isTerminalsArr,
                   isNonterminal,
                   isNonterminals,
                   isNonterminalsArr,
                   isStartSymbInNonTerm,
                   checkIfContains,
                   ) where

import Parse
import Struct
import Data.Char

-------------------------------------------------------------------
-- Validate correctness of grammar from input
-- Returns bool value, True if grammar is correct, otherwise False
-------------------------------------------------------------------
bkgValidation :: BKGStruct -> Bool
bkgValidation bkg
    |not (null ntBKG) && not (null sBKG) &&
     isNonterminalsArr ntBKG &&
     isTerminalsArr tBKG &&
     isNonterminal sBKG &&
     isStartSymbInNonTerm sBKG ntBKG &&
     checkRules tBKG (ntBKG ++ ["#"]) rBKG = True
    |otherwise = False
    where    ntBKG = nonTerminals bkg
             tBKG = terminals bkg
             sBKG = startSymbol bkg
             rBKG = rules bkg


-------------------------------------------------------------------------------------------------------------------
-- First paramerter [String] = For example list of nonterminals
-- Second parameter [String] = For example list of terminals
-- Third parameter [String] = Left or right part of rule, for example array of both nonterminals and terminals
-- Retruns True if all chars in third parameter are subset of concatenation chars of first and second parameters
-- If not, then returns False
-------------------------------------------------------------------------------------------------------------------
checkIfContains :: [String] -> [String] ->  [String] -> Bool
checkIfContains _ _ [] = False
checkIfContains acceptNonTerm acceptTerm [x] =  x `elem` acceptNonTerm || x `elem` acceptTerm
checkIfContains acceptNonTerm acceptTerm (x:xs) = (x `elem` acceptNonTerm || x `elem` acceptTerm) && checkIfContains acceptNonTerm acceptTerm xs


-----------------------------------------------------
-- Validate correctness of grammar rules from input
-----------------------------------------------------
checkRules :: [String] -> [String] -> [String] -> Bool
checkRules _ _ [] = True
checkRules term nonTerm [x] = isNonterminals (head (parseRule x)) && head (parseRule x) `elem` nonTerm && checkIfContains nonTerm term (stringToCharsList (head (tail (parseRule x))))
checkRules term nonTerm (x:xs) = (isNonterminals (head (parseRule x)) && head (parseRule x) `elem` nonTerm && checkIfContains nonTerm term (stringToCharsList (head (tail (parseRule x))))) && checkRules term nonTerm xs


-------------------------------------------------------------------------------------------
-- ***supportive function*** for isLowerOrHashtag
-- Checks output value from isLower function, if True then returns True too
-- Otherwise checks if char is hashtag, if yes then returns True, otherwise returns False
-------------------------------------------------------------------------------------------
getIsLowerOrHashtagBool :: Bool -> Char -> Bool
getIsLowerOrHashtagBool y x
    | y = True
    | x == '#' = True
    | otherwise = False 


---------------------------------------------------------------------------
-- Returns True if char is lower case or hashtag, otherwise returns false
---------------------------------------------------------------------------
isLowerOrHashtag :: Char -> Bool
isLowerOrHashtag x = getIsLowerOrHashtagBool (isLower x) x

--

------------------------------------------------------------
-- Returns True if char is hashtag, otherwise returns False
------------------------------------------------------------
isHashtag :: Char -> Bool
isHashtag x
    | x == '#' = True
    | otherwise = False 


-----------------------------------------------------------------------------
-- Returns True if if string contains only hashtags, otherwise returns False
-----------------------------------------------------------------------------
ifHastagOnly :: String -> Bool
ifHastagOnly = foldr ((&&) . isHashtag) True


-------------------------------------------------------------------------------------
-- Returns True if if items in array contains only hashtags, otherwise returns False
-------------------------------------------------------------------------------------
ifHastagOnlyArr :: [String] -> Bool
ifHastagOnlyArr [] = True
ifHastagOnlyArr [[]] = True
ifHastagOnlyArr (x:xs) = ifHastagOnly x && ifHastagOnlyArr xs


--


-------------------------------------------------------------------------------------------------
-- ***supportive function***
-- Returns empty array if bool is True (it means all rules contains only hashtags on right side)
-- Otherwise returns array (it means there is at least one rule with terminal on right side)
-------------------------------------------------------------------------------------------------
ifRulesAreOnlyHastag :: Bool -> [String] -> [String]
ifRulesAreOnlyHastag x y
    | x = []
    | otherwise = y


----


---------------------------------------------------------------------------
-- Return True if char is lower case (terminals), otherwise returns False
---------------------------------------------------------------------------
isTerminal :: String -> Bool
isTerminal [] = False
isTerminal (x:_xs) = isLower x


--------------------------------------------------------------------------------------------------------
-- Returns True if all chars in strings are lower case (terminals) or hashtag, otherwise returns False
--------------------------------------------------------------------------------------------------------
isTerminals :: String -> Bool
isTerminals = foldr ((&&) . isLowerOrHashtag) True

----------------------------------------------------------------------------------------------
-- Returns True if if items in array contains terminals or hashtags, otherwise returns False
----------------------------------------------------------------------------------------------
isTerminalsArr :: [String] -> Bool
isTerminalsArr [] = True
isTerminalsArr [[]] = True
isTerminalsArr (x:xs) = isTerminals x && isTerminalsArr xs

-- 

------------------------------------------------------------------------------
-- Return True if char is upper case (nonterminals), otherwise returns False
------------------------------------------------------------------------------
isNonterminal :: String -> Bool
isNonterminal [] = False
isNonterminal (x:_xs) = isUpper x

----------------------------------------------------------------------------------------------------------
-- Returns True if all chars in strings are upper case (nonterminals) or hashtag, otherwise returns False
----------------------------------------------------------------------------------------------------------
isNonterminals :: String -> Bool
isNonterminals = foldr ((&&) . isUpper) True


------------------------------------------------------------------------------------------------
-- Returns True if if items in array contains nonterminals or hashtags, otherwise returns False
------------------------------------------------------------------------------------------------
isNonterminalsArr :: [String] -> Bool
isNonterminalsArr [] = True
isNonterminalsArr [[]] = True
isNonterminalsArr (x:xs) = isNonterminals x && isNonterminalsArr xs

--

isStartSymbInNonTerm :: Eq a => a -> [a] -> Bool
isStartSymbInNonTerm = elem
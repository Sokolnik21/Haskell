module Lib
    ( linesQuan,
      wordsQuan,
      signsQuan,
      wordsDiff,
      linesLongerThan80
    ) where

import Data.List

linesQuan :: String -> Int
wordsQuan :: String -> Int
signsQuan :: String -> Int
wordsDiff :: String -> Int
linesLongerThan80 :: String -> Int

linesQuan list = length $ lines list
wordsQuan list = length $ words list
signsQuan list = length list
wordsDiff list = length $ nub $ words list
linesLongerThan80 list = length $ filter (\x -> (length x) > 80) $ lines list


-- -- old implementation
-- -- more Haskellish but less clear
-- indicNewLine = ['\n']
-- indicNewWord = [' ', '\t', '\n']
--
-- linesQuan list = (+ 1) $ length $ filter (\x -> elem x indicNewLine) list
-- wordsQuan list = (+ 1) $ length $ filter (\x -> elem x indicNewWord) list
-- signsQuan list = foldl (\acc x -> acc + 1) 0 list

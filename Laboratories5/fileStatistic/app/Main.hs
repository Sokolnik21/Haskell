module Main where

import System.Environment
import System.IO
import Control.Exception

import Lib

makeStatisticLine :: Show a => String -> (t -> a) -> t -> IO ()
makeStatisticLine prefix f text = putStr prefix >> print (f text)

data ErrorType = NoArg | TooManyArgs deriving Show

data MyException = MkMyException ErrorType deriving Show

instance Exception MyException

checkArgs [] = throw (MkMyException NoArg)
checkArgs list
  | tail list /= []   = throw (MkMyException TooManyArgs)
  | otherwise         = list

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      contents <- readFile (head args)
      makeStatisticLine "Lines: " linesQuan contents
      makeStatisticLine "Words: " wordsQuan contents
      makeStatisticLine "Signs: " signsQuan contents
      makeStatisticLine "Diff words: " wordsDiff contents
      makeStatisticLine "Lines longer than 80: " linesLongerThan80 contents
    []  -> throw (MkMyException NoArg)
    _   -> throw (MkMyException TooManyArgs)

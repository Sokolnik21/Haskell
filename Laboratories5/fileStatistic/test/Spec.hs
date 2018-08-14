import Test.HUnit

import Lib

testText = "Ala ma kota.\nAla ma psa"

test1 = TestCase (assertEqual "Test1 " 2 (linesQuan testText))
test2 = TestCase (assertEqual "Test2 " 6 (wordsQuan testText))
test3 = TestCase (assertEqual "Test3 " 23 (signsQuan testText))
test4 = TestCase (assertEqual "Test4 " 4 (wordsDiff testText))
test5 = TestCase (assertEqual "Test5 " 0 (linesLongerThan80 testText))

tests = TestList [
  TestLabel "test1" test1,
  TestLabel "test2" test2,
  TestLabel "test3" test3,
  TestLabel "test4" test4,
  TestLabel "test5" test5
  ]

main :: IO Counts
main = do runTestTT tests

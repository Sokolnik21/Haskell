import Test.QuickCheck
import Test.HUnit

import Stack
import Dequeue

main = do { runTestTT testsHUnit
          ; putStr "\nTest QuickCheck\n"
          ; quickCheck prop_dequeue_creat_take_front
          ; quickCheck prop_dequeue_creat_take_back }

-- HUnit
test1 = TestCase (assertBool  "Test1 " (isEmpty empty))
test2 = TestCase (assertEqual "Test2 " "MkStack [1]" (show $ push 1 empty))
test3 = TestCase (assertEqual "Test3 " "(1, MkStack [])" (show $ pop $ push 1 empty))
test4 = TestCase (assertEqual "Test4 " "1" (show $ top $ push 1 empty))

test5 = TestCase (assertBool  "Test5 " (isEmptyDEQ emptyDEQ))
test6 = TestCase (assertEqual "Test6 " "MkDequeue [1]" (show $ pushFrontDEQ emptyDEQ 1))
testDeq = fromListDEQ [1..3]
test7 = TestCase (assertEqual "Test7 " "MkDequeue [1,2,3]" (show testDeq))
test8 = TestCase (assertEqual "Test8 " "Just 1" (show $ firstDEQ testDeq))
test9 = TestCase (assertEqual "Test9 " "Just 3" (show $ lastDEQ testDeq))

testsHUnit = TestList [
  TestLabel "test1" test1,
  TestLabel "test2" test2,
  TestLabel "test3" test3,
  TestLabel "test4" test4,
  TestLabel "test5" test5,
  TestLabel "test6" test6,
  TestLabel "test7" test7,
  TestLabel "test8" test8,
  TestLabel "test9" test9
  ]

-- QuickCheck
prop_dequeue_creat_take_front :: [Int] -> Bool
prop_dequeue_creat_take_front xs = takeFrontDEQ (lengthDEQ instDeq) instDeq == xs
  where instDeq = fromListDEQ xs
prop_dequeue_creat_take_back :: [Int] -> Bool
prop_dequeue_creat_take_back xs = takeBackDEQ (lengthDEQ instDeq) instDeq == reverse xs
  where instDeq = fromListDEQ xs

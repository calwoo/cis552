-- Problem: Define a function, called listIncr, that, 
-- given a list of ints, returns a new list where each number has been incremented.

import Test.HUnit

-- define tests
listIncrTests :: Test
listIncrTests =
  TestList [ listIncr [1,2,3] ~?= [2,3,4],
             listIncr [42]    ~?= [43] ]

-- define function
listIncr :: [Int] -> [Int]
listIncr [] = []
listIncr (x:xs) = (x+1) : listIncr xs

-- run tests
runLITests :: IO Counts
runLITests = runTestTT listIncrTests
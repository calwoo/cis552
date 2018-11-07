-- Problem: Define a function, called listAdd, that, given a list of Ints returns their sum.

import Test.HUnit

-- define tests
listAddTests :: Test
listAddTests = TestList [ listAdd [1,2,3] ~?= 6,
                          listAdd [] ~?= 0 ]

-- define function
listAdd :: [Int] -> Int
listAdd [] = 0
listAdd (x:xs) = x + listAdd xs

-- run tests
runLATests :: IO Counts
runLATests = runTestTT listAddTests
-- Problem: Define a function that, given two integers i and j, 
-- returns a list containing all of the numbers at least as big as i but no bigger than j, in order.

import Test.HUnit

-- test cases
testRange :: Test
testRange = TestList [ range 3  6  ~?= [3,4,5,6],
                       range 42 42 ~?= [42],
                      range 10 5  ~?= [] ]

-- define function
range :: Int -> Int -> [Int]
range i j
    | i <= j = i : range (i+1) j
    | i > j = []

-- run tests
runRTests :: IO Counts
runRTests = runTestTT testRange
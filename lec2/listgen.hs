-- Problem: Write a function that, given an argument x and a number n, returns a list containing n copies of x.

import Test.HUnit


-- test cases
testClone1, testClone2, testClone3 :: Test
testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]
testClone4 = clone 'a' (-1) ~?= []

-- define function
clone :: a -> Int -> [a]
clone x n
    | n <= 0 = []
    | otherwise = x : clone x (n-1)

-- run tests
cls :: IO Counts
cls = runTestTT (TestList [
    testClone1, testClone2, testClone3, testClone4 ])
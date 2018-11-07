{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

{-# OPTIONS -fdefer-type-errors  #-}

module Main where
import Prelude hiding (takeWhile, all, zip, reverse, concat)
import Test.HUnit
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Text.Read as Read

main :: IO ()
main = do
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testWeather,
                               testSoccer ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzip ]

abc :: Bool -> Bool -> Bool -> Bool
abc x y z =
  x && y || (x && z)
 

tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True,
                          abc True False False ~?= False,
                          abc False True True ~?= False]

arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic x1 x2 =
     let a = fst (fst x1) in
     let b = snd (fst x1) in
     let c = snd x1 in
     let d = fst (fst x2) in
     let e = snd (fst x2) in
     let f = snd x2
       in
       (b*f - c*e, c*
       d - a*f
       , a*e-b*d)
 

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3),
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]

reverse l  = reverse_aux l [] where
  reverse_aux l acc =
    if null l then acc
       else reverse_aux (tail l) (head l : acc)
 

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

zip = g 0 where
  g n xs ys = if n == length xs || n == length ys then [] else
          (xs !! n, ys !! n) : g (n + 1) xs ys

tzip :: Test
tzip = "zip" ~:
  TestList [ zip "abc" [True,False,True] ~?= [('a',True),('b',False), ('c', True)],
             zip "abc" [True] ~?= [('a', True)],
             zip [] [] ~?= ([] :: [(Int,Int)]) ]

--------------------------------------------------------------------------------

testLists :: Test
testLists = "testLists" ~: TestList
  [tintersperse, tinvert, ttranspose, tconcat, tcountSub]

-- The intersperse function takes an element and a list
-- and intersperses that element between the elements of the list.
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"
--
-- intersperse is defined in Data.List, and you can test your solution against
-- that one.

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse c (x:xs) = x : c : intersperse c xs

tintersperse :: Test
tintersperse = intersperse ',' "abcde" ~?= "a,b,c,d,e"
 

-- invert lst returns a list with each pair reversed.
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--

invert :: [(a,b)] -> [(b,a)]
invert = map (\(x,y) -> (y,x))

tinvert :: Test
tinvert = TestList [ invert [("a",1),("a",2)] ~?= [(1,"a"),(2,"a")],
                     invert ([] :: [(Int,Char)]) ~?= [] ]
 

-- concat

-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
--
-- NOTE: remember you cannot use any functions from the Prelude or Data.List for
-- this problem, even for use as a helper function.

concat :: [[a]] -> [a]
concat = foldr (++) []

tconcat :: Test
tconcat = concat [[1,2,3],[4,5,6],[7,8,9]] ~?= [1,2,3,4,5,6,7,8,9]

-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument.
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is *not* the same behavior as the library version
-- of transpose.

-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
--    transpose  [[]] returns []
-- transpose is defined in Data.List

transpose :: Eq a => [[a]] -> [[a]]
transpose mat = let (column, rest) = grab mat
                in case column of
                      [] -> []
                      _ -> column : transpose rest
                where
                  grab :: Eq a => [[a]] -> ([a], [[a]])
                  grab mat = if [] `List.elem` mat
                              then ([], [])
                              else (map List.head mat, map List.tail mat)

ttranspose :: Test
ttranspose = TestList [ transpose [[1,2,3],[4,5,6]] ~?= [[1,4],[2,5],[3,6]],
                        transpose [[1,2],[3,4,5]] ~?= [[1,3],[2,4]],
                        transpose ([[]] :: [[Int]]) ~?= [] ]

-- countSub sub str

-- Return the number of (potentially overlapping) occurrences of substring sub
-- found in the string str.
-- for example:
--      countSub "aa" "aaa" returns 2

countSub :: String -> String -> Int
countSub substr str = 
  counter substr str 0
    where 
      counter :: String -> String -> Int -> Int
      counter substr str count =
            let l = List.length substr in
              if List.take l str == substr
              then counter substr (List.tail str) (count+1)
              else if str == []
                  then count
                  else counter substr (List.tail str) count

tcountSub :: Test
tcountSub = countSub "aa" "aaa" ~?= 2

--------------------------------------------------------------------------------

-- Part One: Hottest Day

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

weather :: String -> String
weather str = 
  let cleaned = cleanWeather str in
    lines cleaned
    |> map words
    |> map (!!! [0..2])
    |> map (map readInt)
    |> minTempSpread (0,1000)
    |> show

minTempSpread :: (Int, Int) -> [[Maybe Int]] -> Int
minTempSpread (minInt, _) [] = minInt
minTempSpread (minInt, minSpread) (x:xs) =
  let [ind, max, min] = x in
    let Just minner = min
        Just maxxer = max
        Just inder = ind
        in if (maxxer - minner) < minSpread
            then minTempSpread (inder, maxxer-minner) xs
            else minTempSpread (minInt, minSpread) xs

cleanWeather :: String -> String
cleanWeather str =
   concat $ intersperse "\n" $ take 31 $ drop 18 (lines str)

(!!!) :: [a] -> [Int] -> [a]
(!!!) xs [] = []
(!!!) xs (i:is) = (xs !! i) : (xs !!! is)

weatherProgram :: IO ()
weatherProgram = do
  str <- readFile "jul17.dat"
  putStrLn (weather str)

readInt :: String -> Maybe Int
readInt = Read.readMaybe

testWeather :: Test
testWeather = "weather" ~: do str <- readFile "jul17.dat"
                              weather str @?= "6"

--------

-- Part Two: Soccer League Table

soccer :: String -> String
soccer = error "unimplemented"
 

soccerProgram :: IO ()
soccerProgram = do
  str <- readFile "soccer.dat"
  putStrLn (soccer str)

testSoccer :: Test
testSoccer = "soccer" ~: do
  str <- readFile "soccer.dat"
  soccer str @?= "Aston_Villa"

-- Part Three: DRY Fusion

weather2 :: String -> String
weather2 = undefined

soccer2 :: String -> String
soccer2 = undefined

-- Kata Questions

-- To what extent did the design decisions you made when writing the original
-- programs make it easier or harder to factor out common code?

shortAnswer1 :: String
shortAnswer1 = "Fill in your answer here"

-- Was the way you wrote the second program influenced by writing the first?

shortAnswer2 :: String
shortAnswer2 = "Fill in your answer here"

-- Is factoring out as much common code as possible always a good thing? Did the
-- readability of the programs suffer because of this requirement? How about the
-- maintainability?

shortAnswer3 :: String
shortAnswer3 = "Fill in your answer here"




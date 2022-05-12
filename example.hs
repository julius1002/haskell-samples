{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List
import System.Console.Terminfo

type Firstname = String

type Lastname = String

type Age = Int

data Gender
  = Female
  | Male
  deriving (Show)

data Person = Person Firstname Lastname Age Gender deriving (Show)

person1 :: Person
person1 = Person "Julius" "Platon" 25 Male

firstName :: Person -> Firstname
firstName (Person a _ _ _) = a

sumList :: [Int] -> Int
sumList = sum

persons :: [Person]
persons = [Person "Max" "Mustermann" 18 Male, Person "Sabine" "Musterfrau" 12 Female]

getAge :: Person -> Int
getAge (Person _ _ age _) = age

-- Where exapmle
getsDiscount :: [Person] -> Bool
getsDiscount list = ageSum < 30
  where
    ageSum = sum (map getAge list)

-- Local functions
getsDiscountLocal :: [Person] -> Bool
getsDiscountLocal list = sum (map getAge list) < 30
  where
    getAge (Person _ _ age _) = age

-- Let example
getsDiscountLet :: [Person] -> Bool
getsDiscountLet list =
  let ageSum = sum (map getAge list)
   in ageSum < 30

-- pattern matching
complicated :: (Bool, [a1], Bool, [a2]) -> [a1]
complicated (True, x : xs, False, []) = (xs)

-- record syntax
data Animal = Animal
  { name :: String,
    age :: Int,
    race :: String,
    weight :: Int
  }
  deriving (Show)

animal = Animal {name = "Tiger", age = 15, race = "Grosskatze", weight = 150}

-- Maybe
safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs =
  if null (tail xs)
    then Nothing
    else Just (head (tail (xs)))

safeSecondPm :: [a] -> Maybe a
safeSecondPm (_ : x : _) = Just x
safeSecondPm _ = Nothing

-- Guards
lend3 :: (Ord a, Fractional a) => a -> a -> Maybe a
lend3 amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where
    reserve = 100
    newBalance = balance - amount

myLength :: [a] -> Int
myLength = foldr (\x -> (+) 1) 0

calculateMean :: [Int] -> Int
calculateMean list = listSum `div` length
  where
    listSum = sum list
    length = myLength list

sortByLengthOfSubList :: [[a]] -> [[a]]
sortByLengthOfSubList = sortBy compareBySubListLength

compareBySubListLength :: [a] -> [a] -> Ordering
compareBySubListLength list1 list2
  | length list1 > length list2 = GT
  | length list2 > length list1 = LT
  | otherwise = EQ

headWithValidation :: [Char] -> Char
headWithValidation [] = 'Z'
headWithValidation (x : _) = x

isComma :: Char -> Bool
isComma c = c == ','

isCommaLambda :: Char -> Bool
isCommaLambda = (== ',')

isCommaSection :: Char -> Bool
isCommaSection = (== ',')

sumListx :: [Int] -> Int
sumListx (x : xs) = foldl (+) 0 (x : xs)

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

-- map (*3) [1,2,3,4,5,6]

halve :: [a] -> ([a], [a])
halve list = ((take half list), drop half list)
  where
    half = length list `div` 2

third :: [a] -> a
third (_ : _ : c : xs) = c

safetail :: [a] -> [a]
safetail list
  | null list = []
  | otherwise = tail list

mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

lamdbaMult :: Int -> Int -> Int -> Int
lamdbaMult = \x -> \y -> \z -> x * y * z

grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x, y) | x <- [0 .. x], y <- [0 .. y]]

square :: Int -> [(Int, Int)]
square n = filter (uncurry (/=)) (grid n n)

myReplicate :: Int -> a -> [a]
myReplicate n el = [el | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], (x ^ 2 + y ^ 2) == z ^ 2]

myConcat :: [[a]] -> [a]
myConcat = foldl (++) []

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n -1) xs

add :: Int -> Int -> Int
add = \x -> \y -> x + y

-- partial function application minimal example
addTwo :: Int -> Int
addTwo = add 2

nestedList = [[1 .. 6], [3 .. 10]]

mappedList = map (map (+ 1)) nestedList

myAll :: (a -> Bool) -> [a] -> Bool
myAll p list = foldl (&&) True (map p list)

act :: IO (Char, Char)
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

guessWordsGame :: IO ()
guessWordsGame = do
  putStrLn "Think of a word:"
  word <- getLine
  putStrLn "Try to guess it:"
  play word

play :: String -> IO ()
play word = do
  putStr "?"
  guess <- getLine
  if word == guess
    then putStrLn "You won"
    else do
      putStrLn "Try again"
      play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- typeclasses and instances

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True = "This is the boolean value of truth"

class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)

instance BasicEq3 Color where
  isEqual3 Red Red = True
  isEqual3 Green Green = True
  isEqual3 Blue Blue = True
  isEqual3 _ _ = False

-- nim game

next :: Int -> Int
next 1 = 2
next 2 = 1

initial :: [Int]
initial = [5, 4, 3, 2, 1]

finished :: [Int] -> Bool
finished = all (== 0)

valid :: Int -> Int -> [Int] -> Bool
valid index number board
  | finished board = False
  | index > (length board -1) = False
  | (board !! index) < number = False
  | otherwise = True

-- functionally update element of list with given index
move :: [Int] -> Int -> Int -> [Int]
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (replicate num '*')

putBoard :: [Int] -> IO ()
putBoard [a, b, c, d, e] = do
  putRow 1 a
  putRow 2 b
  putRow 3 c
  putRow 4 d
  putRow 5 e

filterMap :: (t -> a) -> (t -> Bool) -> [t] -> [a]
filterMap f p (x : xs) = [f x | x <- xs, p x]

filterMap1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filterMap1 f p list = map f (filter p list)

all1 :: (a -> Bool) -> [a] -> Bool
all1 p xs = foldr ((||) . p) False xs

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 p (x : xs)
  | p x = x : takeWhile1 p xs
  | otherwise = takeWhile1 p xs
takeWhile1 p [] = []

module First where

import Data.List

-- 1.a

fst' (a, b) = a

snd' (a, b) = b

take' 0 a = []
take' a [] = []
take' a (x:xs) = x : take' (pred a) xs

--- take 2 [1,2,3] = 1
--- take 1 [2,3] 2
--- take 0 [3] = []

drop' 0 a = a
drop' a [] = []
drop' a (x:xs) = drop' (a-1) xs

--- drop 2 [1,2,3,4,5] = [2,3,4,5]
--- drop 1 [2,3,4,5] = [3,4,5]
--- drop 0 [3,4,5] = [3,4,5]

null' [] = True
null' _ = False

max' a b
  | a >= b = a
  | otherwise = b

min' a b
  | a <= b = a
  | otherwise = b

length' [] = 0
length' (x:xs) = 1 + length' xs

--- length' [1,2,3] = 3
--- length' [2,3] = 2
--- length' [3] = 1
--- length' [] = 0

head' (x:xs) = x

tail' (x:xs) = xs

last' [x] = x
last' (x:xs) = last' xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--- reverse' [1,2,3,4] = 4,

elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

notElem' a [] = True
notElem' a (x:xs)
  | a == x = False
  | otherwise = notElem' a xs

init' [x] = []
init' (x:xs) = x : init' xs

or' [] = False
or' (x:xs)
  | x == True = True
  | otherwise = or' xs

and' [] = True
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

product' [] = 1
product' (x:xs) = x * product' xs

sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum' xs

map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

delete' _ [] = []
delete' 0 (x:xs) = x : xs
delete' a (x:xs)
  | a == x = xs
  | otherwise = x : delete' a xs

deleteAll' _ [] = []
deleteAll' a (x:xs)
  | a == x = deleteAll' a xs
  | otherwise = x : deleteAll' a xs

intersperse' _ [] = []
intersperse' a (x:xs) = x : a : intersperse' a xs

intercalate' _ [x] = x
intercalate' a (x:xs) = x ++ a ++ (intercalate' a xs)

zip' _ [] = []
zip' [] (x:xs) = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip3' [] _ _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3' xs ys zs

takeWhile' _ [] = []
takeWhile' b (x:xs)
  | b x == False = []
  | b x == True = x : takeWhile' b xs

dropWhile' _ [] = []
dropWhile' b (x:xs)
  | b x == True = dropWhile' b xs
  | b x == False = (x:xs)

all' _ [] = True
all' b (x:xs)
  | b x == False = False
  | otherwise = all' b xs

any' _ [] = False
any' b (x:xs)
  | b x == True = True
  | otherwise = any' b xs

maximum' [a] = a
maximum' (x:xs) = max' (x) (maximum' xs)

minimum' [a] = a
minimum' (x:xs) = min' (x) (minimum' xs)

replicate' 0 _ = []
replicate' a b = b : replicate' (a-1) b

insert' a [] = [a]
insert' a (x:xs)
  | a <= x = a : (x:xs)
  | a > x = x : insert' a xs

concat' [] = []
concat' [x] = x
concat' (x:xs) = x ++ concat' xs

words' [] = []
words' (x:xs) = [x] ++ words' (xs)

concatMap' f [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs

sort' [] = []
sort' [a] = [a]
sort' (x:xs)
  | x == minimum (x:xs) = [x] ++ (sort' xs)
  | x > head xs  = sort' (xs ++ [x])
  | otherwise = sort' (xs ++ [x])

splitAt' _ [] = ([],[])
splitAt' _ [x] = ([x],[])
splitAt' a (x:xs) = ((take a (x:xs)), (drop a (x:xs)))

inits' [] = [[]]
inits' (x:xs) = (inits' (init (x:xs))) ++ [(x:xs)]

tails' [] = [[]]
tails' (x:xs) = [(x:xs)] ++ tails' xs

--tails [1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], []]

maximum'' [x] = x
maximum'' (x:xs)
  | [x] > xs = x
  | otherwise = maximum'' xs

minimum'' [x] = x
minimum'' (x:xs)
  | [x] < xs = x
  | otherwise = minimum'' xs

--foldl f a [] = []
--foldl f a (x:xs) = f a x

--foldl1' f [a] = a
--foldl1' f (x:xs) = f x (foldl1' f xs)

-- scanl (+) 2 [1,2,3,4,5] = [2,3,5,8,12,17]
-- scanl a : f a x : scanl

--scanl' f a [] = [a]
--scanl' f a (x:xs) = [a] ++ [f a x] ++ (scanl' f a xs)

--[a] ++ [f a x] ++ [f a (scanl'' f a (x:xs))]
-- char = string
-- "" --> list of char
-- '' --> char

nub' [] = []
nub' (x:xs) = x : nub' (deleteAll' x (x:xs))

union' [] [] = []
union' _ [] = _
union' (x:xs) (y:ys) = 

-- map, filter, delete, deleteAll,
--foldl, foldl1, zip, zipWith, nth, scanl, scanl1,
-- reverse, intersperse, intercalate, zip3
-- sum, product, words, lines, unlines, unwords,
-- takeWhile, dropWhile, concatMap, all, any, insert, zipWith3

-- 1.b nub, sort, minimum, maximum, inits, tails, union, intersect
-- group, splitAt, partition, replicate

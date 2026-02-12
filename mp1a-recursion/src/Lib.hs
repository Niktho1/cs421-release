--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
-- if Int <= 0, return []
-- if length() < Int, return array
-- else return Int elements
mytake _ [] = []
mytake n _ | n <= 0 = []
mytake n (a:as) = a : mytake (n-1) as

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n a | n <= 0 = a
mydrop n (a:as) = mydrop (n-1) as

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = help xs []
    where 
        help [] ys = ys
        help (x:xs) ys = help xs (x:ys)


--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app xs [] = xs
app [] ys = ys
app (x:xs) ys = x : app xs ys

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist xs = help xs 0
    where
        help [] a = a
        help (x:xs) a = help xs (a+x)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys =
    let p = myzip xs ys
    in myadd p
    where
        myadd [] = []
        myadd ((a,b) : ps) = (a+b) : myadd ps


--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = inc 0
    where
        inc a = a : inc (a+1)

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (P.tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add i [] = [i]
add i (x:xs) | i < x = i : x : xs | i > x = x : add i xs | otherwise = x:xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union x [] = x
union [] y = y
union (x:xs) (y:ys) 
    | x < y = x : union xs (y:ys) 
    | y < x = y : union (x:xs) ys 
    | otherwise = x : union xs ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect x [] = []
intersect [] y = []
intersect (x:xs) (y:ys) 
    | x == y = x : intersect xs ys 
    | x < y = intersect xs (y:ys) 
    | otherwise = intersect (x:xs) ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union ps (help ps)
    where
        ps = powerset xs
        help [] = []
        help (y:ys) = add x y : help ys

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' x = P.map (+1) x

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' x = P.foldl (+) 0 x

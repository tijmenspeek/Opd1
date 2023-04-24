
module Opd1 where
import Data.Bits

--1
faca::Int->Int
faca 0 = 1
faca n = n * faca (n-1)

facb:: Int -> Int
facb n
 | n == 0 = 1
 | otherwise = n * facb(n-1)


 --2a
 
-- b^2 -4ac
-- -b +- sqrt d /2a
nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c = (-b - sqrt(b^2 - 4 * a * c)) / (2 * a) : (-b + sqrt(b^2 - 4 * a * c)) / (2 * a) : []

-- 2b
nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
  | d > 0 = x
  | d == 0 = (-b + sqrt(b^2 - 4 * a * c)) / (2 * a) : []
  | otherwise = []
  where
    d = b^2 - 4 * a * c
    x = (-b - sqrt(b^2 - 4 * a * c)) / (2 * a) : (-b + sqrt(b^2 - 4 * a * c)) / (2 * a): []

     

-- 2c

worpen :: [(Int, Int, Int)]
worpen = [(x, y, z) | x <- [1..6], y <- [1..6], z <- [1..6], (x + y + z) `mod` 5 == 0]

--2d
worpen2 :: Int -> [(Int, Int, Int)]
worpen2 n = [(x, y, z) | x <- [1..6], y <- [1..6], z <- [1..6], (x + y + z) `mod` n == 0]

--3

findNumbers :: (Int, Int, Int)
findNumbers = head [(a,b,c) | c <- [0..1000], b <- [0..c], a <- [2 * (b - c)], b == a * c, c == (a + b) `div` 2]
--4a

mult::Integer->Integer->Integer
mult x 0 = 0
mult x y =  x + (mult x (y - 1))


--4b

fastmult :: Int -> Int -> Int
fastmult x y = fastmultHelper x y 0
  where
    fastmultHelper 0 _ acc = acc
    fastmultHelper x y acc
      | odd x  = fastmultHelper (x `shiftR` 1) (y `shiftL` 1) (acc + y)
      | otherwise = fastmultHelper (x `shiftR` 1) (y `shiftL` 1) acc


--5a

pow :: Int -> Int -> Int
pow _ 0 = 1
pow x e
  | e > 0 = x * pow x (e-1)
  | otherwise = 1

--5b
fastpow x y = fastpowHelper x y 1
  where 
    fastpowHelper _ 0 acc = acc
    fastpowHelper x y acc
      | odd y = fastpowHelper (x * x) (y `shiftR` 1) (acc*x)
      | otherwise = fastpowHelper (x * x) (y `shiftR` 1) acc

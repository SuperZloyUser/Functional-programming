module Appendix where

import Data.Bits


-- Функция, переводящая в двоичную систему счисления из десятичной
int2binary :: Int -> [Int]
int2binary n 
    | (n < 2) = [sc !! n]
    | otherwise = (int2binary (n `div` 2)) ++ [sc !! (n `mod` 2)]
        where sc = [0, 1]

-- Функция, переводящая в десятичную систему счисления из двоичной
binary2int :: [Int] -> Int -> Int
binary2int a b
    | length a == 0 = 0
    | otherwise = (binary2int (init a) (b + 1)) + (last a) * (2 ^ b)


-- Функция, разворачивающая список для циклического сдвига
cycleRotate :: [a] -> Int -> [a]
cycleRotate [] _ = []
cycleRotate xs n = zipWith const (drop n (cycle xs)) xs


-- Функция, выполняющая логический сдвиг
logShift :: Int -> Int -> Int
logShift a b = shift a b

-- Функция, выполняющая циклический сдвиг
cycleShift :: Int -> Int -> Int
cycleShift a b = if b > 0
        then
          do binary2int (cycleRotate (int2binary a) b) 0
        else 
          do binary2int (cycleRotate (int2binary a) (abs ((length (int2binary a)) - b))) 0 


-- Функция, выполняющая арифметический сдвиг
arifShift :: Int -> Int -> Int
arifShift a b = if b > 0
        then
          do logShift a b
        else 
          do (logShift a b) + (2 ^ ((length (int2binary a)) - 1))
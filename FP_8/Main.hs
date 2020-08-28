{-
Разработать программу, которая распределяет работу на несколько потоков выполнения и завершает 
свою работу только по окончании всех потоков. Программа должна быть устойчива к некорректному пользовательскому вводу. 
Обеспечить максимальное покрытие кода тестами.

1. По возможности следует использовать стандартные Haskell-модули.

2. Программа должна компилироваться и запускаться как штатными средствами 
операционной системы, так и утилитой runhaskell.

3. Обеспечить поддержку аргументов командной строки. Минимально: справка по использованию программы 
при запуске без аргументов. Остальные аргументы - на усмотрение разработчика.

4. Должны быть написаны тесты для основных функциональных блоков программы. 
Обеспечить максимальное покрытие кода тестами.

Вариант 6
Структура данных: кафедра; количество преподавателей; количество профессоров. 
Создать два запроса, позволяющих определить кафедры, где нет профессоров, и кафедры, в которых их доля максимальна
-}

module Main where

import System.IO
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.Maybe
import Text.Read

import Appendix


main :: IO ()
main = do
          args <- getArgs
          if (((length args) == 2) && ((read (args !! 0)::Int) > 0))
              then
                do
                number <- setter (args !! 0)
                shift_number <- setter (args !! 1)
          
                l <- newEmptyMVar
                c <- newEmptyMVar
                a <- newEmptyMVar
    
                forkIO $ do 
                    putMVar l (logShift number shift_number)
                rl <- takeMVar l
    
                forkIO $ do 
                    putMVar c (cycleShift number shift_number)
                rc <- takeMVar c
    
                forkIO $ do 
                    putMVar a (arifShift number shift_number)
                ra <- takeMVar a

                putStr "Binary code: "
                print (int2binary number)   
                putStr "Logical shift result: "
                print rl
                putStr "Cycle shift result: "
                print rc
                putStr "Arithmetic shift result: "
                print ra
                putStr "----------------------------\n"

                exitSuccess
            else
              do
              putStr "\nIncorrect arguments!\n\n"
              putStr "Manual: runhaskell (program name) (number) (shift_number: positive to shift left, negative - otherwise)\n"
              putStr "Example: runhaskell FP_8.hs 3 2\n\n"
              putStr "Menu:\n"
              putStr "1 - Enter numbers manually.\n"
              putStr "0 - Exit\n"
              x <- getLine
              when (x /= "0") $ do 
              case x of
                "1"     -> caller args
                _       -> main
          exitSuccess


-- Функция, вызывающая вычисления сдвигов
caller :: [String] -> IO()
caller args = do

    putStrLn "Enter number: "
    number <- inputPosInteger

    putStrLn "Enter shift: "
    shift_number <- inputInteger
    putStr "\n"

    l <- newEmptyMVar
    c <- newEmptyMVar
    a <- newEmptyMVar
    
    forkIO $ do 
        putMVar l (logShift number shift_number)
    rl <- takeMVar l
    
    forkIO $ do 
        putMVar c (cycleShift number shift_number)
    rc <- takeMVar c
    
    forkIO $ do 
        putMVar a (arifShift number shift_number)
    ra <- takeMVar a
    
    putStr "Binary code: "
    print (int2binary number)
    putStr "Logical shift result: "
    print rl
    putStr "Cycle shift result: "
    print rc
    putStr "Arithmetic shift result: "
    print ra
    putStr "----------------------------\n"

    exitSuccess


setter :: String -> IO(Int)
setter a = return (read a)


-- Функция, считывающая положительное число
inputPosInteger :: IO Int
inputPosInteger = do
    int <- getLine
    case readMaybe int of
        Just x ->  if (x > 0)
                    then return x
                   else  putStrLn "Enter a positive number:" >> inputPosInteger
        Nothing -> putStrLn "Incorrect input" >> inputPosInteger


-- Функция, считывающая число
inputInteger :: IO Int
inputInteger = do
    int <- getLine
    case readMaybe int of
        Just x ->  return x
        Nothing -> putStrLn "Incorrect input" >> inputInteger
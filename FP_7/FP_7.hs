{- 
Модифицировать код, созданный при выполнении ЛР 6 следующим образом:

1) Разбить на модули по "областям ответственности". 
	По возможности использовать стандартные Haskell-модули.

2) Программа должна компилироваться и запускаться как штатными средствами 
	операционной системы, так и утилитой runhaskell.

3) Обеспечить поддержку аргументов командной строки. 
	Минимально: имена входного и выходного файлов и справка 
	по использованию программы при запуске без аргументов. 
	Остальные аргументы - на усмотрение разработчика.

Вариант 12
Структура данных: кафедра; количество преподавателей; количество профессоров. 
Создать два запроса, позволяющих определить кафедры, где нет профессоров, и кафедры, в которых их доля максимальна
-}

module Main where

import System.IO
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Applicative
import Control.Monad
import Appendix

main :: IO ()
main = do
          args <- getArgs
          print "Manual: runhaskell (program name) (input file) (output file)"
          print "Example: runhaskell FP_7.hs 1 1"
          print "Menu:"
          print "1 - Add department"
          print "2 - Delete department"
          print "3 - Change department data"
          print "4 - Output departments without professors."
          print "5 - Output departments that have the maximum part of professors."
          print "6 - Output departments that have the minimum part of professors but not empty."
          print "7 - Read departments data from file"
          print "8 - Write departments data to file"
          print "0 - Exit"
          x <- getLine
          when (x /= "0") $ do 
          case x of
            "1"     -> print =<< addDepartment
            "2"     -> print =<< superDelete
            "3"     -> print =<< interactiveModify
            "4"     -> print =<< interactiveFR
            "5"     -> print =<< interactiveSR
            "6"     -> print =<< interactiveTR
            "7"     -> print =<< readFromFile args
            "8"     -> writeToFile args
            _       -> return ()
          main 

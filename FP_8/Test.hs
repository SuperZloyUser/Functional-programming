{-
Для установки библиотеки HUnit на Windows необходимо обновить утилиту cabal 
до последней версии с помощью "cabal update", далее соответственно установить 
библиотеку HUnti с помощью "cabal install HUnit".

Для запуска тестов необходимо открыть файл Test.hs с помощью 
GHCi и написать "runTestTT tests"
-}


module Test where

import Main
import Appendix

import Test.HUnit

test1 = TestCase (assertEqual "Error int2binary"
           [1,0,1,0]
           (int2binary 10))

test2 = TestCase (assertEqual "Error binary2int"
           10
           (binary2int [1,0,1,0] 0))

test3 = TestCase (assertEqual "Error LogRight"
           1
           (logShift 10 (-3)))
 
test4 = TestCase (assertEqual "Error LogLeft"
           80
           (logShift 10 3))

test5 = TestCase (assertEqual "Error CycleRight"
           5
           (cycleShift 10 (-3)))

test6 = TestCase (assertEqual "Error CycleLeft"
           5
           (cycleShift 10 3))

test7 = TestCase (assertEqual "Error ArifRight"
           9
           (arifShift 10 (-3)))

test8 = TestCase (assertEqual "Error ArifLeft"
           80
           (arifShift 10 3))


test9 = TestCase (assertEqual "Error Rotate"
           [3,4,5,1,2]
           (cycleRotate [1,2,3,4,5] 2))

 
tests = TestList[TestLabel "testInt2Binary" test1,
                 TestLabel "testBinary2Int" test2,
                 TestLabel "testLogRight" test3,
                 TestLabel "testLogLeft" test4,
                 TestLabel "testCycleRight" test5,
                 TestLabel "testCycleLeft" test6,
                 TestLabel "test ArifRight" test7,
                 TestLabel "test ArifLeft" test8,
                 TestLabel "test Rotate" test9]
                 
 
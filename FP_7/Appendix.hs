module Appendix where

import Data.List
import System.Directory (doesFileExist)

-- Описание структуры данных (кафедра: её название, количество профессоров и преподавателей на ней)
data Department = Department String Integer Integer deriving (Show, Read)

-- Геттер количества профессоров
getCOP :: Department -> Integer
getCOP (Department _ _ x) = x

-- Геттер количества учителей
getCOT :: Department -> Integer
getCOT (Department _ x _) = x

-- Функция, считывающая список
inputList :: IO [Department]
inputList = do
    list <- readLn
    return list

-- Ещё одна функция, считывающая список
inputListForWTF :: IO [Department]
inputListForWTF = do
    print "Input the list of departments which need write in the file: "
    list <- readLn
    return list

-- Функция, считывающая строку
inputString :: IO String
inputString = do
    str <- getLine
    return (str)

-- Функция, считывающая число
inputInt :: IO Integer
inputInt = do
   int <- getLine
   return (read int)

-- Функция-конструктор для экземпляров структуры
createDepartment :: String -> Integer -> Integer -> Department
createDepartment pName pCountOfTeachers pCountOfProfessors
  | pName == [] = error "Name is empty"
  | pCountOfProfessors < 0 || pCountOfTeachers < 0 = error "Counts can't be less than zero!"
  | pCountOfProfessors > pCountOfTeachers = error "Count of professors can't be more than a count of teachers!"
  | otherwise = Department pName pCountOfTeachers pCountOfProfessors
  
  
-- ***Функця, добавляющая в список структур ещё одну
addDepartment :: IO [Department]
addDepartment = do
    print "Enter the list of departments: "
    list <- inputList
    print "Enter name of department: "
    name <- inputString
    print "Enter count of teachers: "
    teachers <- inputInt
    print "Enter count of professors: "
    professors <- inputInt
    return (list ++ [Department name teachers professors])

-- ***Функции, удаляющие элемент из списка
superDelete :: IO [Department]
superDelete = do
    print "Enter the list of departments: "
    list <- inputList
    print "Enter the number of the department what you want to delete: "
    number <- inputInt
    return (deleteDepartment (fromIntegral number) list)

deleteDepartment :: Int -> [Department] -> [Department]
deleteDepartment number departments
  | number <= 0 || length departments < number = error "Wrong input!"
  | otherwise = take (number - 1) departments ++ drop number departments
--
  
-- ***Функции, изменяющие элемент из списка
interactiveModify :: IO [Department]
interactiveModify = do    
    print "Enter the list of departments: "
    list <- inputList
    print "Enter number of department what you want to change: " 
    number <- inputInt
    print "Enter name of department: "
    name <- inputString
    print "Enter count of teachers: "
    teachers <- inputInt
    print "Enter count of professors: "
    professors <- inputInt
    return (modifyDepartment (fromIntegral number) (Department name teachers professors) list) 

modifyDepartment :: Int -> Department -> [Department] -> [Department]
modifyDepartment number department departments
  | number <= 0 || length departments < number = error "Wrong input!"
  | otherwise = take (number - 1) departments ++ [department] ++ drop number departments
--
 
-- Функция-чекер, проверяющая наличие профессоров на кафедре
isProfEmpty :: Department -> Bool
isProfEmpty department
  | getCOP department == 0 = True
  | otherwise = False
  
-- Функция-чекер, сравнивающая доли профессоров на кафедрах
maxProfPerc :: Department -> Department -> Ordering
maxProfPerc dep1 dep2
  | firstPerc == secondPerc = EQ
  | firstPerc < secondPerc  = GT
  | firstPerc > secondPerc  = LT
    where 
      firstPerc  = fromIntegral (getCOP dep1) / fromIntegral (getCOT dep1)
      secondPerc = fromIntegral (getCOP dep2) / fromIntegral (getCOT dep2)
  
-- Функция высшего порядка, пробегающая по списку и формирующая новый список, если выполняется условие f(Department -> Bool)
listChecker :: [Department] -> (Department -> Bool) -> [Department]
listChecker x y = superListChecker x y 1
  where
    superListChecker :: [Department] -> (Department -> Bool) -> Int -> [Department]
    superListChecker departments f number
      | length departments < number = departments
      | f (head (take number departments)) = superListChecker departments f (number + 1)
      | otherwise = superListChecker (deleteDepartment number departments) f (number)
  
-- Функция, вычисляющая количество кафедр с минимальой долей философов
countOfDep :: [Department] -> Integer
countOfDep a
  | length a == 0 = error "Empty list!"
  | length a == 1 && getCOP (head a) == 0 = 0
  | length a == 1 = 1
  | getCOP (last a) == 0 = countOfDep (init a)
  | firstPerc == secondPerc = countOfDep (init a) + 1
  | otherwise = 1
    where 
      firstPerc  = fromIntegral (getCOP (last a)) / fromIntegral (getCOT (last a))
      secondPerc = fromIntegral (getCOP (last (init a))) / fromIntegral (getCOT (last (init a)))


-- ***Функции, обрабатывающие первый запрос(кафедры, где нет профессоров)
interactiveFR :: IO [Department]
interactiveFR = do
    print "Enter the list of departments: "
    list <- inputList
    return (firstRequest list)

firstRequest :: [Department] -> [Department]
firstRequest list = listChecker list isProfEmpty
--

-- ***Функции, обрабатывающие второй запрос(кафедра, где доля профессоров максимальна)
interactiveSR :: IO [Department]
interactiveSR = do
    print "Enter the list of departments: "
    list <- inputList
    return (firstRequest list)

secondRequest :: [Department] -> [Department]
secondRequest list = sortBy maxProfPerc list
--

-- ***Функции, обрабатывающие третий запрос(количество кафедр, где доля профессоров минимальна, но не равна 0)
interactiveTR :: IO [Department]
interactiveTR = do
    print "Enter the list of departments: "
    list <- inputList
    return (firstRequest list)

thirdRequest :: [Department] -> Integer
thirdRequest list = countOfDep (sortBy maxProfPerc list)
--

-- ***Функция, считывающая данные из файла (Сначала проверяется возможноть считать данные из файла,
-- введённого в качестве аргумента программы, если аргументов нет или не удаётся открыть файл, то
-- считывается название файла, из которого необходимо считать данные, в интерактивном режиме)
readFromFile :: [String] -> IO [Department]
readFromFile args = do
    if (length args) > 0
      then
        do
          isFileExist <- doesFileExist (head args)
          if isFileExist
            then
            do
              text <- readFile (head args)
              return $ (map read (lines text))
          else
            do
              print "Arguments is empty or file isn't exists, enter the name of file: "
              fileName <- inputString
              isFileExistS <- doesFileExist fileName
              if isFileExistS
                then
                  do
                    text <- readFile fileName
                    return $ (map read (lines text))
              else
                do
                  print "File isn't exists!"
                  return []
        else
          do
            print "Args is empty or file isn't exists, enter the name of file: "
            fileName <- inputString
            isFileExistS <- doesFileExist fileName
            if isFileExistS
              then
                do
                  text <- readFile fileName
                  return $ (map read (lines text))
            else
              do
                print "File isn't exists!"
                return []
          

-- ***Функция, записывающая данные в файл (Сначала проверяется возможноть записать данные в файл,
-- введённый в качестве аргумента программы, если аргументов нет, то
-- считывается название файла, в который необходимо провести запись данных, в интерактивном режиме)
writeToFile :: [String] -> IO ()
writeToFile args = do
    list <- inputListForWTF
    if ((length args) > 1)
      then
        do
        writeFile (args !! 1) (unlines (map show list))
    else
      do
        print "Empty argumet for output file!"
        print "Enter the name of file: "
        fileName <- inputString
        writeFile fileName (unlines (map show list))

   
{- 
Необходимо на языке Haskell расширить (или переопределить) структуры данных, выполненные в ЛР 4:
А) Определить (или доопределить) типы необходимые типы данных.
Б) Определить класс типа Functor (функтор) для полученных типов данных.
В) Определить класс типа Applicative (аппликативный функтор).

Вариант 12
Структура данных: кафедра; количество преподавателей; количество профессоров. 
Создать два запроса, позволяющих определить кафедры, где нет профессоров, и кафедры, в которых их доля максимальна
-}
import Data.List
import Prelude hiding (Functor, fmap, Applicative,  pure, (<*>))

-- Описание структуры данных (кафедра: её название, количество профессоров и преподавателей на ней)
data Department a = Department String Integer Integer | Function a deriving (Show)


-- Объявление функтора
class Functor f where  
    fmap :: (Integer -> Integer) -> f a -> f b

-- Создание экземпляра функтора
instance Functor Department where
    fmap f (Department n a b) = Department n (f a) (f b)

-- Объявление аппликативного функтора
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (Integer -> Integer) -> f a -> f b

-- Создание экземпляра аппликативного функтора
instance Applicative Department where
    pure a = Function a 
    Function f <*> Department n a b = Department n (f a) (f b)


-- Геттер количества профессоров
getCOP :: Department a -> Integer
getCOP (Department _ _ x) = x

-- Геттер количества учителей
getCOT :: Department a -> Integer
getCOT (Department _ x _) = x

-- Функция-конструктор для экземпляров структуры
createDepartment :: String -> Integer -> Integer -> Department a
createDepartment pName pCountOfTeachers pCountOfProfessors
  | pName == [] = error "Name is empty"
  | pCountOfProfessors < 0 || pCountOfTeachers < 0 = error "Counts can't be less than zero!"
  | pCountOfProfessors > pCountOfTeachers = error "Count of professors can't be more than a count of teachers!"
  | otherwise = Department pName pCountOfTeachers pCountOfProfessors

-- ***Функця, добавляющая в список структур ещё одну
addDepartment :: Department a -> [Department a] -> [Department a]
addDepartment department departments = departments ++ [department]

-- ***Функция, удаляющая элемент из списка
deleteDepartment :: Int -> [Department a] -> [Department a]
deleteDepartment number departments
  | number <= 0 || length departments < number = error "Wrong input!"
  | otherwise = take (number - 1) departments ++ drop number departments
  
-- ***Функция, изменяющая элемент из списка
modifyDepartment :: Int -> Department a -> [Department a] -> [Department a]
modifyDepartment number department departments
  | number <= 0 || length departments < number = error "Wrong input!"
  | otherwise = take (number - 1) departments ++ [department] ++ drop number departments
  
-- Функция-чекер, проверяющая наличие профессоров на кафедре
isProfEmpty :: Department a -> Bool
isProfEmpty department
  | getCOP department == 0 = True
  | otherwise = False
  
-- Функция-чекер, сравнивающая доли профессоров на кафедрах
maxProfPerc :: Department a -> Department a -> Ordering
maxProfPerc dep1 dep2
  | firstPerc == secondPerc = EQ
  | firstPerc < secondPerc  = GT
  | firstPerc > secondPerc  = LT
    where 
      firstPerc  = fromIntegral (getCOP dep1) / fromIntegral (getCOT dep1)
      secondPerc = fromIntegral (getCOP dep2) / fromIntegral (getCOT dep2)
  
-- Функция высшего порядка, пробегающая по списку и формирующая новый список, если выполняется условие f(Department -> Bool)
listChecker :: [Department a] -> (Department a -> Bool) -> [Department a]
listChecker x y = superListChecker x y 1
  where
    superListChecker :: [Department a] -> (Department a -> Bool) -> Int -> [Department a]
    superListChecker departments f number
      | length departments < number = departments
      | f (head (take number departments)) = superListChecker departments f (number + 1)
      | otherwise = superListChecker (deleteDepartment number departments) f (number)
  
-- Функция, вычисляющая количество кафедр с минимальой долей философов
countOfDep :: [Department a] -> Integer
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


-- ***Функция, обрабатывающая первый запрос(кафедры, где нет профессоров)
firstRequest :: [Department a] -> [Department a]
firstRequest list = listChecker list isProfEmpty

-- ***Функция, обрабатывающая второй запрос(кафедра, где доля профессоров максимальна)
secondRequest :: [Department a] -> [Department a]
secondRequest list = sortBy maxProfPerc list

-- ***Функция, обрабатывающая третий запрос(количество кафедр, где доля профессоров минимальна, но не равна 0)
thirdRequest :: [Department a] -> Integer
thirdRequest list = countOfDep (sortBy maxProfPerc list)


test :: [Department a]
test = addDepartment third (addDepartment second (addDepartment first []))
    where 
      first = createDepartment "first" 20 5
      second = createDepartment "second" 100 99
      third = createDepartment "third" 10 0
 
test2 :: [Department a]
test2 = addDepartment third (addDepartment second (addDepartment first []))
    where 
      first = createDepartment "first" 3 1
      second = createDepartment "second" 10 1
      third = createDepartment "third" 3 1

delete1 :: [Department a]
delete1 = deleteDepartment 0 test

delete2 :: [Department a]
delete2 = deleteDepartment 2 test
          
delete3 :: [Department a]
delete3 = deleteDepartment 4 test

modify :: [Department a]
modify = modifyDepartment 2 (createDepartment "fourth" 1 0) test

testFirstRequest :: [Department a]
testFirstRequest = firstRequest test

testThirdRequest :: Integer
testThirdRequest = thirdRequest test2
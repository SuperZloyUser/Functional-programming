processedList :: (Eq a) => [[a]] -> [[a]]
processedList a
  | tail (tail (head a)) == [] = a
  | otherwise = processedList [[(head (head a))] ++ (tail (tail (head a))), (head (tail a)) ++ [head (tail (head a))]]
  
callCenterList :: (Eq a) => [a] -> [[a]]
callCenterList a = processedList ([a, []])

swapFirstLast :: (Eq a) => [a] -> [a]
swapFirstLast a 
  | a == [] = []
  | tail a == [] = a
  | otherwise = (tail (head (callCenterList a))) ++ head ((tail (callCenterList a))) ++ [head (head (callCenterList a))] 

swapFirstOther :: (Eq a) => [a] -> [a]
swapFirstOther a 
  | a == [] = a
  | tail a == [] = a
  | tail (tail a) == [] = a
  | otherwise = init (init (init a)) ++ [last a] ++ [last (init a)] ++ [last (init (init a))]
put :: Show s => s -> IO ()
put = putStrLn . show

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
   where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

main :: IO ()
main = do
  put $ maximum' [2,5,1]
  -- maximum' [2,5,1]
  -- 2 > maximum' [5,1]
  -- 2 > (5 > maximum' [1])
  -- 2 > (5 > 1)
  -- 2 > 5
  -- 5

  put $ maximum'' [2,5,1]
  -- maximum'' [2,5,1]
  -- max 2 (maximum'' [5,1])
  -- max 2 (max 5 maximum'' [1])
  -- max 2 (max 5 1)
  -- max 2 5
  -- 5

  put $ replicate' 3 5
  -- replicate' 3 5
  -- 5:(replicate' (3-1) 5)
  -- 5:(5:(replicate' (2-1) 5))
  -- 5:(5:(5:(replicate' (1-1) 5)))
  -- 5:(5:(5:([])))
  -- [5,5,5]

  put $ take' 3 [5,4,3,2,1]
  -- take' 3 [5,4,3,2,1]
  -- 5:(take' (3-1) [4,3,2,1])
  -- 5:(4:(take' (2-1) [3,2,1]))
  -- 5:(4:(3:(take' (1-1) [2,1])))
  -- 5:(4:(3:([])))
  -- [5,4,3]

  put $ reverse' [1,2,3]
  -- reverse' [1,2,3]
  -- reverse' [2,3] ++ [1]
  -- reverse' [3] ++ [2] ++ [1]
  -- reverse' [] ++ [3] ++ [2] ++ [1]
  -- [] ++ [3] ++ [2] ++ [1]
  -- [3,2,1]

  put $ take' 5 (repeat 3)
  -- take' 5 (repeat 3)
  -- take' 5 (3:(repeat 3))
  -- 3:(take' (5-1) (3:repeat 3))
  -- [3,3,3,3,3]

  put $ zip' [1,2,3] ['a', 'b']
  -- zip' [1,2,3] ['a', 'b']
  -- (1,'a'):zip' [2,3] ['b']
  -- (1,'a'):(2,'b'):zip' [3] []
  -- (1,'a'):(2,'b'):[]
  -- [(1,'a'),(2,'b')]

  put $ elem' 2 [1,2,3]
  -- elem' 2 [1,2,3]
  -- 1 == 2 = 2 `elem'` [2,3]
  -- 2 == 2 = True
  -- True

  put $ elem' 10 [-9,-8,-7]
  -- elem' 10 [-9,-8,-7]
  -- 10 == -9 = 10 `elem'` [-8,-7]
  -- 10 == -8 = 10 `elem'` [-7]
  -- 10 == -7 = 10 `elem'` []
  -- False

  put $ quicksort [10,2,5,3,1,6]
  -- quicksort [10,2,5,3,1,6]
  -- quicksort [2,5,3,1,6] ++ [10] ++ quicksort []
  -- quicksort [2,5,3,1,6] ++ [10] ++ []
  -- (quicksort [1] ++ [2] ++ quicksort [5,3,6]) ++ [10] ++ []
  -- ((quicksort [] ++ [1] ++ quicksort []) ++ [2] ++ quicksort [5,3,6]) ++ [10] ++ []
  -- (([] ++ [1] ++ []) ++ [2] ++ quicksort [5,3,6]) ++ [10] ++ []
  -- ([1] ++ [2] ++ quicksort [5,3,6]) ++ [10] ++ []
  -- ([1] ++ [2] ++ (quicksort [3] ++ [5] ++ quicksort [6])) ++ [10] ++ []
  -- ([1] ++ [2] ++ ([3] ++ [5] ++ [6])) ++ [10] ++ []
  -- ([1] ++ [2] ++ ([3,5,6])) ++ [10] ++ []
  -- ([1,2,3,5,6]) ++ [10] ++ []
  -- [1,2,3,5,6,10]

  put $ quicksort "the quick brown fox jumps over the lazy dog"
  -- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"

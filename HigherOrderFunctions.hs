put :: Show s => s -> IO ()
put = putStrLn . show

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

numLongchains' :: Int
numLongchains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum' :: Integer
oddSquareSum' =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in sum belowLimit

main :: IO ()
main = do
  put $ max 4 5              -- 5
  put $ (max 4) 5            -- 5
  put $ max 4 5 == (max 4) 5 -- True

  let multTwoWithNine = multThree 9
  put $ multTwoWithNine 2 3 -- 54
  let multWithEighteen = multTwoWithNine 2
  put $ multWithEighteen 10 -- 180

  put $ compareWithHundred 99  -- GT
  put $ compareWithHundred' 99 -- GT
  put $ compareWithHundred 99 == compareWithHundred' 99 -- True

  put $ divideByTen 200 -- 20.0
  put $ (/10) 200       -- 20.0

  put $ isUpperAlphanum 'C' -- True
  put $ isUpperAlphanum '@' -- False

  put $ applyTwice (+3) 10            -- 16
  put $ applyTwice (++ " HAHA") "HEY" -- HEY HAHA HAHA
  put $ applyTwice (multThree 2 2) 9  -- 144
  put $ applyTwice (3:) [1]           -- [3,3,1]

  put $ zipWith' (+) [4,2,5,6] [2,6,2,3]
  -- zipWith' (+) [4,2,5,6] [2,6,2,3]
  -- (+) 4 2 : zipWith' (+) [2,5,6] [6,2,3]
  -- (+) 4 2 : (+) 2 6 : zipWith' (+) [5,6] [2,3]
  -- (+) 4 2 : (+) 2 6 : (+) 5 2 : zipWith' (+) [6] [3]
  -- (+) 4 2 : (+) 2 6 : (+) 5 2 : (+) 6 3 : zipWith' (+) [] []
  -- (+) 4 2 : (+) 2 6 : (+) 5 2 : (+) 6 3 : []
  -- 6 : 8 : 7 : 9 : []
  -- [6,8,7,9]
  put $ zipWith' max [6,3,2,1] [7,3,1,5]
  -- [7,3,2,5]
  put $ zipWith' (++) ["foo ","bar ","baz "] ["fighters","hoppers","aldrin"]
  -- ["foo fighters","bar hoppers","baz aldrin"]
  put $ zipWith' (*) (replicate 5 2) [1..]
  -- [2,4,6,8,10]
  put $ zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
  -- [[3,4,6],[9,20,30],[10,12,12]]

  put $ flip' zip [1,2,3,4,5] "hello"
  -- [('h',1),('e',2),('l',3),('l',4),('o',5)]
  put $ zipWith (flip' div) [2,2..] [10,8,6,4,2]
  -- [5,4,3,2,1]

  put $ map (+3) [1,5,3,16]                     -- [4,8,6,4,9]
  put $ map (++ "!") ["BIFF", "BANG", "POW"]    -- ["BIFF!", "BANG!", "POW!"]
  put $ map (replicate 3) [3..6]                -- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
  put $ map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  -- [[1,4],[9,16,25,36],[49,64]]
  put $ map fst [(1,2),(3,5),(6,3),(2,6),(2,5)] -- [1,3,6,2,2]

  put $ filter (>3) [1,5,3,2,1,6,4,3,2,1] -- [5,6,4]
  put $ filter (==3) [1,2,3,4,5]          -- [3]
  put $ filter even [1..10]               -- [2,4,6,8,10]
  put $ let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
 -- [[1,2,3],[3,4,5],[2,2]]
  put $ filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent" -- "uagameasadifeent"
  put $ filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
  -- "GAYBALLS"

  put largestDivisible -- 99554

  put $ takeWhile (/=' ') "elephants know how to party" -- "elephants"

  put $ sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) -- 166650
  put $ sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) -- 166650

  put $ chain 10 -- [10,5,16,8,4,2,1]
  put $ chain 1  -- [1]
  put $ chain 30 -- [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

  put numLongChains -- 66

  let listOfFuns = map (*) [0..]
  put $ (listOfFuns !! 4) 5 -- 20

  put numLongChains -- 66
  put $ zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
  -- [153.0,61.5,31.0,15.75,6.6]
  put $ map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
  -- [3,8,9,8,7]
  put $ addThree 1 2 3 -- 6
  put $ flip''' zip [1..5] ['a'..'z']
  -- [('a',1),('b',2),('c',3),('d',4),('e',5)]

  put $ sum' [3,5,2,1]
  -- sum' [3,5,2,1]
  -- (0 + 3) [5,2,1]
  -- (0 + 3 + 5) [2,1]
  -- (0 + 3 + 5 + 2) [1]
  -- (0 + 3 + 5 + 2 + 1) []
  -- 11

  put $ elem' 3 [2,3,4]
  -- elem' 3 [2,3,4]
  -- (3 == 2) [3,4] -- acc is False
  -- (3 == 3) [3,4] -- acc is True
  -- (3 == 4) [3,4] -- acc is True
  -- True

  put $ map' (+3) [1,2,3]
  -- map' (+3) [1,2,3]
  -- [1,2] (3 + 3) : []
  -- [1] (2 + 3) : (3 + 3) : []
  -- [] (1 + 3) : (2 + 3) : (3 + 3) : []
  -- [] (4) : (5) : (6) : []
  -- [4,5,6]

  put $ scanl (+) 0 [3,5,2,1] -- [0,3,8,10,11]
  put $ scanr (+) 0 [3,5,2,1] -- [11,8,3,1,0]
  put $ scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
  -- [3,4,5,5,7,9,9,9]
  put $ scanl (flip (:)) [] [3,2,1] -- [[],[3],[2,3],[1,2,3]]

  put sqrtSums -- 131
  put $ sum (map sqrt [1..131])
  -- 1005.0942035344083
  put $ sum (map sqrt [1..130])
  -- 993.6486803921487

  put $ map ($ 3) [(4+), (10*), (^2), sqrt] -- [7.0,30.0,9.0,1.7320508075688772]

  put $ map (\x -> negate (abs x)) [5,3,6,7,3,2,19,24]
  -- [-5,-3,-6,-7,-3,-2,-19,-24]
  put $ map (negate . abs) [5,3,6,7,3,2,19,24]
  -- [-5,-3,-6,-7,-3,-2,-19,-24]

  put $ map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
  -- [-14,-15,-27]
  put $ map (negate . sum . tail) [[1..5],[3..6],[1..7]]
  -- [-14,-15,-27]

  put $ replicate 5 (product (map (*3) (zipWith max [1..5] [4..8])))
  -- [1632960,1632960,1632960,1632960,1632960]
  put $ replicate 5 . product . map (*3) . zipWith max [1..5] $ [4..8]
  -- [1632960,1632960,1632960,1632960,1632960]

  let fn = ceiling . negate . tan . cos . max 50
  put $ fn 100 -- -1

  put oddSquareSum  -- 166650
  put oddSquareSum' -- 166650


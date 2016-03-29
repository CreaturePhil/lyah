put :: Show s => s -> IO ()
put = putStrLn . show

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName _   = "Make sure to always include a catch all pattern so program doesn't crash on unexpected input"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is too long. The first two elements: " ++ show x ++ " and "++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise                   = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b    = GT
  | a == b   = EQ
  | otherwise = LT

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

bmiTell''' :: (RealFloat a) => a -> a -> String
bmiTell''' weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."

main :: IO ()
main = do
  put $ lucky 7  -- "LUCKY NUMBER SEVEN!"
  put $ lucky 10 -- "Sorry, you're out of luck, pal!"

  put $ sayMe 1  -- "One!"
  put $ sayMe 3  -- "Three!"
  put $ sayMe 10 -- "Not between 1 and 5"

  put $ factorial 3
  -- factorial 3
  -- 3 * factorial (3 - 1)
  -- 3 * (2 * factorial (2 - 1))
  -- 3 * (2 * (1 * factorial (1 - 1)))
  -- 3 * (2 * (1 * factorial 0))
  -- 3 * (2 * (1 * 1))
  -- 3 * (2 * 1)
  -- 3 * 2
  -- 6

  put $ charName 'a' -- "Albert"
  put $ charName 'b' -- "Broseph"
  put $ charName 'h'
  -- "Make sure to always include a catch all pattern so program doesn't crash on unexpected input"

  put $ addVectors (1,2) (3,4)  -- (4,6)
  put $ addVectors' (1,2) (3,4) -- (4,6)

  let triple = (1, 2, 3)
  put $ first triple  -- 1
  put $ second triple -- 2
  put $ third triple  -- 3

  let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
  put $ [a+b | (a,b) <- xs] -- [4,7,6,8,11,4]

  put $ head' [4,5,6] -- 4
  put $ head' "Hello" -- 'H'

  put $ tell [1]     -- "The list has one element: 1"
  put $ tell [1,2]   -- "The list has two elements: 1 and 2"
  put $ tell [1,2,3] -- "The list is too long. The first two elements: 1 and 2"

  put $ length' [1..10] -- 10
  put $ length' "ham"
  -- length' "ham"
  -- 1 + length' "am"
  -- 1 + (1 + length "m")
  -- 1 + (1 + (1 + length ""))
  -- 1 + (1 + (1 + 0))
  -- 1 + (1 + 1)
  -- 1 + 2
  -- 3

  put $ sum' [5,10]
  -- sum' [5,10]
  -- 5 + sum' [10]
  -- 5 + (10 + sum' [])
  -- 5 + (10 + 0)
  -- 5 + 10
  -- 15

  put $ capital "Dracula" -- "The first letter of Dracula is D"

  put $ bmiTell' 85 1.90 -- "You're supposedly normal. Pffft, I bet you're ugly!"

  put $ max' 3 2        -- 3
  put $ 3 `myCompare` 2 -- GT

  put $ initials "Philip" "La" -- "P. L."

  put $ calcBmis [(1,2),(3,4),(5,6)] -- [0.25,0.1875,0.1388888888888889]

  put $ cylinder 2 2 -- 50.26548245743669

  put $ [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"] -- ["Woo","Bar"]
  put $ 4 * (let a = 9 in a + 1) + 2 -- 42
  put $ [let square x = x * x in (square 5, square 3, square 2)] -- [(25,9,4)]
  put $ (let a = 100; b = 200; c = 300 in a*b*c, let foo = "Hey "; bar = "there!" in foo ++ bar) -- (6000000,"Hey there!")
  put $ (let (a,b,c) = (1,2,3) in a+b+c) * 100

  let zoot x y z = x * y + z
  put $ zoot 3 9 2                               -- 29
  put $ let boot x y z = x * y + z in boot 3 4 2 -- 14

  put $ describeList []     -- "The list is empty."
  put $ describeList [1]    -- "The list is a singleton list."
  put $ describeList [1, 2] -- "The list is a longer list."

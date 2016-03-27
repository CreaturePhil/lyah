import Data.Typeable

put :: Show s => s -> IO ()
put = putStrLn . show

putType :: Typeable a => a -> IO ()
putType = putStrLn . show . typeOf

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

main :: IO ()
main = do
  putType 'a'         -- Char
  putType True        -- Bool
  putType "HELLO!"    -- [Char]
  putType (True, 'a') -- (Bool,Char)
  putType $ 4 == 5    -- Bool

  -- Explicit types are always denoted with the first letter in capital case.

  put $ circumference 4.0  -- 25.132742
  put $ circumference' 4.0 -- 25.132741228718345

  -- => stands for class constraint

  -- Eq
  put $ 5 == 5             -- True
  put $ 5 /= 5             -- False
  put $ 'a' == 'a'         -- True
  put $ "Ho Ho" == "Ho Ho" -- True
  put $ 3.432 == 3.432     -- True

  -- Ord
  put $ "Abrakadabra" < "Zebra"         -- True
  put $ "Abrakadabra" `compare` "Zebra" -- LT
  put $ 5 >= 2                          -- True
  put $ 5 `compare` 3                   -- GT

  -- Show
  put $ show 3     -- "3"
  put $ show 5.334 -- "5.334"
  put $ show True  -- "True"

  -- Read
  put $ read "True" || False    -- True
  put $ read "8.2" + 3.8        -- 12.0
  put $ read "5" - 2            -- 3
  put $ read "[1,2,3,4]" ++ [3] -- [1,2,3,4,3]
  -- type annotations
  put (read "5" :: Int)               -- 5
  put (read "5" :: Float)             -- 5.0
  put $ (read "5" :: Float) * 4       -- 20.0
  put (read "[1,2,3,4]" :: [Int])     -- [1,2,3,4]
  put (read "(3,'a')" :: (Int, Char)) -- (3,'a')

  -- Enum
  put ['a'..'e'] -- "abcde"
  put [LT .. GT] -- [LT,EQ,GT]
  put [3..5]     -- [3,4,5]
  put $ succ 'B' -- 'C'

  -- Bounded
  put (minBound :: Int)               -- -9223372036854775808
  put (maxBound :: Char)              -- '\1114111'
  put (maxBound :: Bool)              -- True
  put (minBound :: Bool)              -- False
  put (maxBound :: (Bool, Int, Char)) -- (True,9223372036854775807,'\1114111')

  -- Num
  put (20 :: Int)     -- 20
  put (20 :: Integer) -- 20
  put (20 :: Float)   -- 20.0
  put (20 :: Double)  -- 20.0

  -- Integral
  -- must be a whole number, has Int and Integer, cannot be a float or double

  -- Floating
  -- includes only floating point numbers, has Float and Double

  -- fromIntegral takes an integer and turns it into a more general number
  -- for example, length returns an Int. length :: [a] -> Int
  put $ fromIntegral (length [1,2,3,4]) + 3.2 -- 7.2

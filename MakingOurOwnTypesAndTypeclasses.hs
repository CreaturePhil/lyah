module Shapes
( Point (..) -- export all value constructors of Point
, Shape (..)
, surface
, nudge
) where

import Data.Typeable
import qualified Data.Map as Map

put :: Show s => s -> IO ()
put = putStrLn . show

putType :: Typeable a => a -> IO ()
putType = putStrLn . show . typeOf

-- Data Types
-- Shape has two value constructors of Circle or Rectangle.
-- Circle has 3 parameters and Rectangle has 4 parameters.
-- Value constructors are actually functions that return a value of a data type.
data Shape = Circle Float Float Float
           | Rectangle Float Float Float Float
           deriving Show

-- Circle and Rectangle are not a type, they are functions. Shape is a type.
-- We can't do Circle -> Float just like we can't do True -> Int.

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float
            | Rectangle' Point Point
            deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x+a) (y+b)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b =
  Rectangle' (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- Remember, value constructors are just functions that take the fields as
-- parameters and return a value of some type (like Shape) as a result.

-- Record Syntax
-- Field names of value constructors become functions
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- Maybe is a type constructor
-- Nothing and Just are value constructors
-- List type also uses a type parameter

data Car' a b c = Car' { company' :: a
                       , model' :: b
                       , year' :: c
                       } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company' = c, model' = m, year' = y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving(Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- When declaring a data type, the part before the = is the type constructor
-- and the constructors after it (possibly separated by |'s)
-- are value constructors.

-- Derived instances
data Person' = Person' { firstName' :: String
                       , lastName' :: String
                       , age' :: Int
                       } deriving (Eq, Show, Read)

-- In Ord typeclass, values are compared by the order LEAST to GREATEST in which
-- they appear. Example is the Bool type in which True > False
-- data Bool = False | True deriving (Ord)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type synonyms give a type another name
-- Ex. type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]

-- Partially applying type parameters
type IntMap = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                              then Right code
                              else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken,"ZD39O"))
  ,(101,(Free,"JAH3I"))
  ,(103,(Free,"IQSA9"))
  ,(105,(Free,"QOTSA"))
  ,(109,(Taken,"893JJ"))
  ,(110,(Taken,"99292"))]

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- types have their own little labels, called kinds
-- Ex. Int :: *
-- A * means that the type is a concrete type.
-- A concrete type is a type that doesn't take any type parameters
-- and values can only have types that are concrete types.

class Tofu t where
  tofu :: j a -> t a j

-- Here, j has to have a kind of * -> *
-- t has to have a kind of * -> (* -> *) -> *
--                         ^        ^
--                      a :: *   j :: * -> *

data Frank a b = Frank {frankField :: b a} deriving (Show)
-- a :: * and b :: * -> *

instance Tofu Frank where
  tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k }
-- something -> something -> something -> *
-- p :: *
-- t :: * -> *
-- k :: *
-- (* -> *) -> * -> * -> *

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

main :: IO ()
main = do
  put $ surface $ Circle 10 20 10       -- 314.15927
  put $ surface $ Rectangle 0 0 100 100 -- 10000.0

  -- Mapping and partially applying a value constructor
  put $ map (Circle 10 20) [4,5,6,6]
  -- [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

  put $ nudge (Circle' (Point 34 34) 10) 5 10 -- Circle' (Point 39.0 44.0) 10.0

  -- Don't have to put the fields in order
  put Car {company="Ford", year=1967, model="Mustang"}
  -- Car {company = "Ford", model = "Mustang", year = 1967}

  put $ Just "Haha"     -- Just "Haha"
  put $ Just 84         -- Just 84
  putType $ Just "Haha" -- Maybe [Char]

  let stang = Car {company="Ford", model="Mustang", year=1967}
  put $ tellCar stang -- "This Ford Mustang was made in 1967"

  put $ tellCar' (Car' "Ford" "Mustang" 1967)
  -- "This Ford Mustang was made in 1967"
  put $ tellCar' (Car' "Ford" "Mustang" "nineteen sixty seven")
  -- "This Ford Mustang was made in \"nineteen sixty seven\""

  put $ Vector 3 5 8 `vplus` Vector 9 2 8 -- Vector 12 7 16
  put $ Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3 -- Vector 12 9 19
  put $ Vector 3 9 7 `vectMult` 10 -- Vector 30 90 70
  put $ Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0 -- 74.0
  put $ Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
  -- Vector 148 666 222

  let mikeD = Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
  let adRock = Person' {firstName' = "Adam", lastName' = "Horovitz", age' = 41}
  let mca = Person' {firstName' = "Adam", lastName' = "Yauch", age' = 44}
  put $ mca == adRock   -- False
  put $ mikeD == adRock -- False
  put $ mikeD == mikeD  -- True
  put $ mikeD == Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
  -- False

  let beastieBoys = [mca, adRock, mikeD]
  put $ mikeD `elem` beastieBoys -- True

  put mikeD -- Person' {firstName' = "Micheal", lastName' = "Diamond", age' = 43}
  put $ "mikeD is " ++ show mikeD
  -- "mikeD is Person' {firstName' = \"Micheal\", lastName' = \"Diamond\", age' = 43}"

  put (read "Person' {firstName' =\"Michael\", lastName' =\"Diamond\", age' = 43}" :: Person')
  -- Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}
  put $ read "Person' {firstName' =\"Michael\", lastName' =\"Diamond\", age' = 43}" == mikeD
  -- True

  put Wednesday                -- Wednesday
  put $ show Wednesday         -- "Wednesday"
  put (read "Saturday" :: Day) -- Saturday

  put $ Saturday == Sunday         -- False
  put $ Saturday == Saturday       -- True
  put $ Saturday > Friday          -- True
  put $ Monday `compare` Wednesday -- LT

  put $ succ Monday                     -- Tuesday
  put $ pred Saturday                   -- Friday
  put [Thursday .. Sunday]              -- [Thursday,Friday,Saturday,Sunday]
  put ([minBound .. maxBound] :: [Day]) -- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

  put $ lockerLookup 101 lockers -- Right "JAH3I"
  put $ lockerLookup 100 lockers -- Left "Locker 100 is already taken!"
  put $ lockerLookup 102 lockers -- Left "Locker number 102 doesn't exist!"
  put $ lockerLookup 110 lockers -- Left "Locker 110 is already taken!"
  put $ lockerLookup 105 lockers -- Right "QOTSA"

  let nums = [8,6,4,1,7,3,5]
  let numsTree = foldr treeInsert EmptyTree nums
  put numsTree
  -- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

  put $ 8 `treeElem` numsTree   -- True
  put $ 100 `treeElem` numsTree -- False
  put $ 1 `treeElem` numsTree   -- True
  put $ 10 `treeElem` numsTree  -- False

  put $ Red == Red                      -- True
  put $ Red == Yellow                   -- False
  put $ Red `elem` [Red, Yellow, Green] -- True
  put $ [Red, Yellow, Green]            -- [Red light,Yellow light,Green light]

  put $ yesno $ length [] -- False
  put $ yesno "haha"      -- True
  put $ yesno ""          -- False
  put $ yesno $ Just 0    -- True
  put $ yesno True        -- True
  put $ yesno EmptyTree   -- False
  put $ yesno []          -- False
  put $ yesno [0,0,0]     -- True

  put $ yesnoIf [] "YEAH!" "NO!"         -- "NO!"
  put $ yesnoIf [2,3,4] "YEAH!" "NO!"    -- "YEAH!"
  put $ yesnoIf True "YEAH!" "NO!"       -- "YEAH!"
  put $ yesnoIf (Just 500) "YEAH!" "NO!" -- "YEAH!"
  put $ yesnoIf Nothing "YEAH!" "NO!"    -- "NO!"

  put $ fmap (*2) [1..3] -- [2,4,6]
  put $ map (*2) [1..3]  -- [2,4,6]

  put $ fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
  -- Just "Something serious. HEY GUYS IM INSIDE THE JUST"
  put $ fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing -- Nothing
  put $ fmap (*2) (Just 200)                             -- Just 400
  put $ fmap (*2) Nothing                                -- Nothing

  put $ fmap (*2) EmptyTree
  put $ fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
  -- Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree

  put $ Frank {frankField = Just "HAHA"}
  -- Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
  put $ Frank {frankField = Node 'a' EmptyTree EmptyTree}
  -- Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
  put $ Frank {frankField = "YES"}
  -- Frank {frankField = "YES"} :: Frank Char []

  put (tofu (Just 'a') :: Frank Char Maybe) -- Frank {frankField = Just 'a'}
  put (tofu ["HELLO"] :: Frank [Char] [])   -- Frank {frankField = ["HELLO"]}
  put (tofu (Just ["HELLO"]) :: Frank [[Char]] Maybe)
  -- Frank {frankField = Just ["HELLO"]}

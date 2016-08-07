import Data.Char
import Data.List
import Control.Applicative

-- We usually mark functions that take anything and return anything as
-- a -> b. r -> a is the same thing, we just used different letters
-- for the type variables.

-- fmap :: (a -> b) -> (r -> a) -> (r -> b) relates to
-- function composition

-- In the same vein, if we write fmap :: (a -> b) -> (f a -> f b),
-- we can think of fmap not as a function that takes one function
-- and a functor and returns a functor, but as a function that
-- takes a function and returns a new function that's just like
-- the old one, only it takes a functor as a parameter and returns
-- a functor as the result. It takes an a -> b function and returns
-- a function f a -> f b. This is called lifting a function.

-- You can think of fmap as either a function that takes a function
-- and a functor and then maps that function over the functor, or
-- you can think of it as a function that takes a function and lifts
-- that function so that it operates on functors. Both views are
-- correct and in Haskell, equivalent.

-- The first functor law states that if we map the id function over
-- a functor, the functor that we get back should be the same as the
-- original functor.

-- The second law says that composing two functions and then mapping
-- the resulting function over a functor should be the same as first
-- mapping one function over the functor and then mapping the other one.

-- If you think of functors as things that output values,
-- you can think of mapping over functors as attaching a transformation
-- to the output of the functor that changes the value.

-- When we do fmap (+3) (*3), we attach the transformation (+3) to the
-- eventual output of (*3). Looking at it this way gives us some
-- intuition as to why using fmap on functions is just composition
-- (fmap (+3) (*3) equals (+3) . (*3), which equals \x -> ((x*3)+3)),
-- because we take a function like (*3) then we attach the transformation
-- (+3) to its output. The result is still a function, only when we give
-- it a number, it will be multiplied by three and then it will go
-- through the attached transformation where it will be added to three.
-- This is what happens with composition.

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter+1) (f x)

-- Remember, when looking functions inside typeclasses or instances
-- they are using pattern matching as well.

-- Applicative functors and the applicative style of doing
-- ``pure f <*> x <*> y <*> ...`` allow us to take a function that
-- expects parameters that aren't necessarily wrapped in functors
-- and use that function to operate on several values that are in
-- functor contexts. The function can take as many parameters as
-- we want, because it's always partially applied step by step between
-- occurences of <*>.

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- If you just want your type signatures to look cleaner and be more
-- descriptive, you probably want type synonyms. If you want to take
-- an existing type and wrap it in a new type in order to make it an
-- instance of a type class, chances are you're looking for a newtype.
-- And if you want to make something completely new, odds are good
-- that you're looking for the data keyword.

main = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line ++ " backwards!"

  line' <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line'

  print $ fmap (*3) (+100) 1          -- 303
  print ((*3) `fmap` (+100) $ 1)      -- 303
  print ((*3) . (+100) $ 1)           -- 303
  print (fmap (show . (*3)) (*100) 1) -- "300"

  print $ fmap (replicate 3) [1,2,3,4] -- [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
  print $ fmap (replicate 3) (Just 4)  -- Just [4,4,4]

  print $ CJust 0 "haha"               -- CJust 0 "haha"
  print $ CJust 100 [1,2,3]            -- CJust 100 [1,2,3]
  print $ fmap (++"ha") (CJust 0 "ho") -- CJust 1 "hoha"
  print $ fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
  -- CJust 2 "hohahe"
  print $ fmap (++"blah") CNothing -- CNothing

  print $ fmap id (CJust 0 "haha") -- CJust 1 "haha"
  print $ id (CJust 0 "haha")      -- CJust 0 "haha"

  let a = fmap (*) [1,2,3,4]
  print $ fmap (\f -> f 9) a -- [9,18,27,36]

  print $ Just (+3) <*> Just 9          -- Just 12
  print $ pure (+3) <*> Just 10         -- Just 13
  print $ pure (+3) <*> Just 9          -- Just 12
  print $ Just (++"hahah") <*> Nothing  -- Nothing

  print $ pure (+) <*> Just 3 <*> Just 5  -- Just 8
  -- ^ this is same as (pure (+) <*> Just 3) <*> Just 5
  -- <*> is left-associative
  print $ pure (+) <*> Just 3 <*> Nothing -- Nothing
  print $ pure (+) <*> Nothing <*> Just 5 -- Nothing

  -- pure f <*> x == fmap f x
  print $ (pure (+1) <*> (Just 1)) == (fmap (+1) (Just 1)) -- True

  -- <$> is fmap as an infix operator.
  print $ (++) <$> Just "johntra" <*> Just "volta"
  -- Just "johntravolta"
  print $ (++) "johntra" "volta"
  -- "johntravolta"

  print $ (pure "Hey" :: [String])     -- ["Hey"]
  print $ (pure "Hey" :: Maybe String) -- Maybe String

  print $ [(*0),(+100),(^2)] <*> [1,2,3] -- [0,0,0,101,102,103,1,4,9]
  print $ [(+),(*)] <*> [1,2] <*> [3,4]  -- [4,5,5,6,3,4,6,8]
  print $ (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
  -- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

  print [x*y | x <- [2,5,10], y <- [8,10,11]]
  -- [16,20,22,40,50,55,80,100,110]
  print $ (*) <$> [2,5,10] <*> [8,10,11]
  -- [16,20,22,40,50,55,80,100,110]
  print $ filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
  -- [55,80,100,110]

  a <- myAction
  putStrLn $ "The two lines concatenated turn out to be: " ++ a

  return "The last line in a do statement with return shows up"

  print $ pure 3 "blah" -- 3
  -- This is same as (pure 3) "blah" as pure 3 returns (\_ -> 3)
  print $ (+) <$> (+3) <*> (*100) $ 5 -- 508
  -- (\x -> (+3) x ((*100) x)) 5
  -- (\5 -> (+3) 5 ((*100) 5))
  -- (3+5) (100*5)
  -- (+8) (500)
  -- 508

  print $ liftA2 (:) (Just 3) (Just [4]) -- Just [3,4]
  print $ (:) <$> Just 3 <*> Just [4]    -- Just [3,4]

  print $ sequenceA [Just 3, Just 2, Just 1]  -- Just [3,2,1]
  print $ sequenceA [Just 3, Nothing, Just 1] -- Nothing
  print $ sequenceA [(+3),(+2),(+1)] 3        -- [6,5,4]

  print $ sequenceA [[1,2,3],[4,5,6]]
  -- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

  print $ sequenceA [[1,2,3],[4,5,6],[3,4,4],[]] -- []
  print $ sequenceA [(>4),(<10),odd] 7           -- [True,True,True]

  print $ CharList "this will be shown!"
  print $ CharList {getCharList = "this will also be shown!"}
  print $ helloMe undefined

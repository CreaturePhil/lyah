applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right+n)

x -: f = f x

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                    = Nothing

routine :: Maybe Pole
routine = case landLeft' 1 (0,0) of
  Nothing -> Nothing
  Just pole1 -> case landRight' 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft' 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft' 1 pole3

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

routine' :: Maybe Pole
routine' = do
  start <- return (0,0)
  first <- landLeft' 2 start
  second <- landRight' 2 first
  landLeft' 1 second

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

main :: IO ()
main = do
  print $ (\x -> Just (x+1)) 1   -- Just 2
  print $ (\x -> Just (x+1)) 100 -- Just 101

  print $ Just 3 `applyMaybe` \x -> Just (x+1) -- Just 4
  print $ Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
  -- Just "smille :)"
  print $ Nothing `applyMaybe` \x -> Just (x+1)        -- Nothing
  print $ Nothing `applyMaybe` \x -> Just (x ++ " :)") -- Nothing

  print $ Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing
  -- Just 3
  print $ Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing
  -- Nothing

  print (return "WHAT" :: Maybe String)   -- Just "WHAT"
  print $ Just 9 >>= \x -> return (x*10)  -- Just 90
  print $ Nothing >>= \x -> return (x*10) -- Nothing

  print $ landLeft 2 (0,0)     -- (2,0)
  print $ landRight 1 (1,2)    -- (1,3)
  print $ landRight (-1) (1,2) -- (1,1)

  print $ landLeft 2 (landRight 1 (landLeft 1 (0,0))) -- (3,1)

  print $ 100 -: (*3)         -- 300
  print $ True -: not         -- False
  print $ (0,0) -: landLeft 2 -- (2,0)

  print $ (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2 -- (3,1)

  print $ landLeft' 2 (0,0)  -- Just (2,0)
  print $ landLeft' 10 (0,3) -- Nothing

  print $ landRight' 1 (0,0) >>= landLeft' 2 -- Just (2,1)
  print $ Nothing >>= landLeft' 2             -- Nothing
  print $ return (0,0) >>= landRight' 2 >>= landLeft' 2 >>= landRight' 2
  -- Just (2,4)
  print $ return (0,0) >>= landLeft' 1 >>= landRight' 4 >>= landLeft' (-1) >>= landRight' (-2)
  -- Nothing

  print $ Nothing >> Just 3 -- Nothing
  print $ Just 3 >> Just 4  -- Just 4

  print $ return (0,0) >>= landLeft' 1 >> Nothing >>= landRight' 1
  -- Nothing

  print $ routine -- Just (4,4)

  print $ Just 3 >>= (\x -> Just (show x ++ "!")) -- Just "3!"
  print $ Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
  -- Just "3!"
  print $ let x = 3; y = "!" in show x ++ y -- "3!"

  print foo  -- Just "3!"
  print foo' -- Just "3!"

  print $ Just 9 >>= (\x -> Just (x > 8)) -- Just True
  print $ marySue                         -- Just True

  print routine' -- Just (3,2)
  
  print justH    -- Just 'H'

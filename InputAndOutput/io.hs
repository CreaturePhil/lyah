import Control.Monad

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
  putChar x
  putStr xs

main :: IO ()
main = do
  putStr "Hey, "
  putStr "I'm "
  putStr "Andy!"

  putChar 't'
  putChar 'e'
  putChar 'h'

  putStr' "\n"

  print True
  print 2
  print "haha"
  print 3.2
  print [3,4,3]

  a <- getLine
  b <- getLine
  c <- getLine
  print [a,b,c]
  -- same as
  rs <- sequence [getLine, getLine, getLine]
  print rs

  sequence (map print [1,2,3,4,5])

  mapM print [1,2,3]
  mapM_ print [1,2,3]

  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
    color <- getLine
    return color)
    -- above 2 lines are same as just doing getLine
  putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
  mapM putStrLn colors
  return ()

 -- While return takes a value and wraps it up in a box, <- takes a box
 -- (and performs it) and takes the value out of it, binding it to a name.

main = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line

  a <- return "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

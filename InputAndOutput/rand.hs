import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

main :: IO ()
main = do
  print (random (mkStdGen 100) :: (Int, StdGen))
  print (random (mkStdGen 100) :: (Int, StdGen))
  print (random (mkStdGen 10) :: (Int, StdGen))
  print (random (mkStdGen 0) :: (Int, StdGen))

  print $ threeCoins (mkStdGen 21)
  print $ threeCoins (mkStdGen 22)
  print $ threeCoins (mkStdGen 943)
  print $ threeCoins (mkStdGen 944)

  print (take 5 $ randoms (mkStdGen 11) :: [Int])
  print (take 5 $ randoms (mkStdGen 11) :: [Bool])
  print (take 5 $ randoms (mkStdGen 11) :: [Float])

  print (randomR (1,6) (mkStdGen 359353) :: (Int, StdGen))
  print (randomR (1,6) (mkStdGen 35935335) :: (Int, StdGen))
  print (take 10 $ randomRs ('a', 'z') (mkStdGen 3) :: [Char])

  gen <- getStdGen
  print $ take 20 (randomRs ('a','z') gen)

  let randomChars = randomRs ('a','z') gen
      (first20, rest) = splitAt 20 randomChars
      (second20, _) = splitAt 20 rest
  print first20
  print second20

  gen' <- newStdGen
  print $ take 20 (randomRs ('a','z') gen')

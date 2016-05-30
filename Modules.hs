import Data.List
-- import Data.List (nub, sort)
-- only import selected functions, types, typeclasses

-- import Data.List hiding (nub)
-- import all functions, types, typeclasses except nub

-- import qualified Data.List
-- this means it is namespace so using nub would be Data.List.nub

-- import qualified Data.Map as M
-- this allows us to rename the module so it would be M.nub

put :: Show s => s -> IO ()
put = putStrLn . show

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

main :: IO ()
main = do
  put $ numUniques [1, 2, 1, 3] -- 3

  put $ intersperse '.' "MONKEY"    -- "M.O.N.K.E.Y"
  put $ intersperse 0 [1,2,3,4,5,6] -- [1,0,2,0,3,0,4,0,5,0,6]

  put $ intercalate " " ["hey","there","guys"] -- "hey there guys"
  put $ intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
  -- [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

  put $ transpose [[1,2,3],[4,5,6],[7,8,9]] -- [[1,4,7],[2,5,8],[3,6,9]]
  put $ transpose ["hey","there","guys"]    -- ["htg","ehu","yey","rs","e"]
  put $ map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] -- [18,8,6,17]

  put $ concat ["foo","bar","car"]       -- "foobarcar"
  put $ concat [[3,4,5],[2,3,4],[2,1,1]] -- [3,4,5,2,3,4,2,1,1]
  put $ concatMap (replicate 4) [1..3]   -- [1,1,1,1,2,2,2,2,3,3,3,3]

  put $ and $ map (>4) [5,6,7,8]    -- True
  put $ and $ map (==4) [4,4,4,3,4] -- False

  put $ or $ map (==4) [2,3,4,5,6,1] -- True
  put $ or $ map (>4) [1,2,3]        -- False

  put $ takeWhile (>3) [10,9..1]                  -- [10,9,8,7,6,5,4]
  put $ takeWhile (/=' ') "This is a sentence"    -- "This"
  put $ sum $ takeWhile (<10000) $ map (^3) [1..] -- 53361

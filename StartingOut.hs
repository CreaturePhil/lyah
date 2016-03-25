put :: Show s => s -> IO ()
put s = putStrLn $ show s

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleUs' x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                      then x
                      else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

-- functions can only begin with lowercase letters.

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

main :: IO ()
main = do
  put $ 2 + 15      -- 17
  put $ 49 * 100    -- 4900
  put $ 1892 - 1472 -- 420
  put $ 5 / 2       -- 2.5

  put $ (50 * 100) - 4999 -- 1
  put $ 50 * 100 - 4999   -- 1
  put $ 50 * (100 - 4999) -- -244950

  put $ True && False      -- False
  put $ True && True       -- True
  put $ False || True      -- True
  put $ not False          -- True
  put $ not (True && True) -- False

  put $ 5 == 5             -- True
  put $ 1 == 0             -- False
  put $ 5 /= 5             -- False
  put $ 5 /= 4             -- True
  put $ "hello" == "hello" -- True

  put $ succ 8 -- 9

  put $ min 9 10    -- 9
  put $ min 3.4 3.2 -- 3.2
  put $ max 100 101 -- 101

  put $ (succ 9 + max 5 4 + 1) == ((succ 9) + (max 5 4) + 1) -- True

  put $ doubleMe 9   -- 18
  put $ doubleMe 8.3 -- 16.6

  put $ doubleUs 4 9      -- 26
  put $ doubleUs 2.3 34.2 -- 73.0
  put $ doubleUs 28 88    -- 478

  put conanO'Brien
  -- "It's a-me, Conan O'Brien!"

  let lostNumbers = [4,8,15,16,23,42]
  put $ lostNumbers -- [4,8,15,16,23,42]

  put $ [1,2,3,4] ++ [9,10,11,12] -- [1,2,3,4,9,10,11,12]
  put $ "hello" ++ " " ++ "world" -- "hello world"
  put $ ['w', 'o'] ++ ['o', 't']  -- "woot"

  put $ 'A':" SMALL CAT" -- "A SMALL CAT"
  put $ 5:[1,2,3,4,5]    -- [5,1,2,3,4,5]

  put $ "Steve Buscemi" !! 6            -- 'B'
  put $ [9.4,33.2,96.2,11.2,23.25] !! 1 -- '33.2'

  let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
  put b
  -- [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
  put $ b ++ [[1,1,1,1]]
  -- [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
  put $ [6,6,6]:b
  -- [[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
  put $ b !! 2
  -- [1,2,2,3,4]

  put $ [3,2,1] > [2,1,0]    -- True
  put $ [3,2,1] > [2,10,100] -- True
  put $ [3,4,2] > [3,4]      -- True
  put $ [3,4,2] > [2,4]      -- True
  put $ [3,4,2] == [3,4,2]   -- True

  let xs = [5,4,3,2,1]
  put $ head xs -- 5
  put $ tail xs -- [4,3,2,1]
  put $ last xs -- [1]
  put $ init xs -- [5,4,3,2]

  put $ length xs  -- 5
  put $ null xs    -- False
  put $ null []    -- True
  put $ reverse xs -- [1,2,3,4,5]

  put $ take 3 xs      -- [5,4,3]
  put $ take 1 [3,9,3] -- [3]
  put $ take 5 [1,2]   -- [1,2]
  put $ take 0 [6,6,6] -- []

  put $ drop 3 [8,4,2,1,5,6] -- [1,5,6]
  put $ drop 0 [1,2,3,4]     -- [1,2,3,4]
  put $ drop 100 [1,2,3,4]   -- [1,2,3,4]

  put $ minimum [8,4,2,1,5,6] -- 1
  put $ maximum [1,9,2,3,4]   -- 9

  put $ sum [5,2,1,6,3,2,5,7]     -- 31
  put $ product [6,2,1,2]         -- 24
  put $ product [1,2,5,6,7,9,2,0] -- 0

  put $ 4 `elem` [3,4,5,6]  -- True
  put $ 10 `elem` [3,4,5,6] --  False

  put [1..20]
  -- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
  put ['a'..'z']
  -- "abcdefghijklmnopqrstuvwxyz"
  put ['K'..'Z']
  -- "KLMNOPQRSTUVWXYZ"

  put [2,4..20]
  -- [2,4,6,8,10,12,14,16,18,20]
  put [3,6..20]
  -- [3,6,9,12,15,18]
  put [0.1,0.3..1]
  -- [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]

  put $ take 10 $ cycle [1,2,3]
  -- [1,2,3,1,2,3,1,2,3,1]
  put $ take 12 $ cycle "LOL "
  -- "LOL LOL LOL "
  put $ take 10 $ repeat 5
  -- [5,5,5,5,5,5,5,5,5,5]

  put [x*2 | x <- [1..10]]
  -- [2,4,6,8,10,12,14,16,18,20]
  put [x*2 | x <- [1..10], x*2 >= 12]
  -- [12,14,16,18,20]
  put [x | x <- [50..100], x `mod` 7 == 3]
  -- [52,59,66,73,80,87,94]

  put $ boomBangs [7..13]
  -- ["BOOM!","BOOM!","BANG!","BANG!"]

  put [x | x <- [10..20], x /= 13, x /= 15, x /= 19]
  -- [10,11,12,14,16,17,18,20]
  put [x*y | x <- [2,5,10], y <- [8,10,11]]
  -- [16,20,22,40,50,55,80,100,110]
  put [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
  -- [55,80,100,110]

  let nouns = ["hobo","frog","pope"]
  let adjectives = ["lazy","grouchy","scheming"]
  put [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
  -- ["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog","grouchy pope","scheming hobo","scheming frog","scheming pope"]

  put $ removeNonUppercase "Hahaha! Ahahaha!"
  put $ removeNonUppercase "IdontLIKEFROGS"

  let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
  put [[x | x <- xs, even x] | xs <- xxs]
  -- [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]

  put $ fst (8,11)         -- 8
  put $ fst ("Wow", False) -- "Wow"
  put $ snd (8,11)         -- 11
  put $ snd ("Wow", False) -- False

  put $ zip [1,2,3,4,5] [5,5,5,5,5]
  -- [(1,5),(2,5),(3,5),(4,5),(5,5)]
  put $ zip [1..5] ["one","two","three","four","five"]
  -- [(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
  put $ zip [5,3,2,6,7,2,5,4,6,6] ["im","a","turtle"]
  -- [(5,"im"),(3,"a"),(2,"turtle")]
  put $ zip [1..] ["apple","orange","cherry","mango"]
  -- [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

  let triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
  put triangles
  let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2 == c^2]
  let rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b]
                        , a^2+b^2 == c^2
                        , a+b+c==24
                        ]
  put rightTriangles' -- [(6,8,10)]

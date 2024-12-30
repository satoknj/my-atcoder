import Data.List (group, sort)

main :: IO ()
main = do
  line <- getLine
  putStrLn $ if solve line then "Yes" else "No"

-- |
-- >>> solve "aabbcc"
-- True
-- >>> solve "zzzzzz"
-- False
-- >>> solve "aab"
-- False
-- >>> solve "aabbaa"
-- False
-- >>> solve "abab"
-- False
solve :: String -> Bool
solve x = c1 && c2 && c3
  where c1 = ((== 0) . (`mod` 2) . length) x
        c2 = all (uncurry (==)) $ groupPairs x
        c3 = all ((== 2) . length) $ (group . sort) x

-- |
-- >>> groupPairs "aabb"
-- [('a','a'),('b','b')]
-- >>> groupPairs "aabbc"
-- [('a','a'),('b','b')]
groupPairs :: [a] -> [(a,a)]
groupPairs [] = []
groupPairs [x] = []
groupPairs (x:y:xs) = (x,y) : groupPairs xs

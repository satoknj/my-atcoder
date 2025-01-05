import Data.List (group, sort, sortBy, sortOn)

main :: IO ()
main = do
  line <- getLine
  putStrLn $ if solve line then "Yes" else "No"

-- まずは[('a',1)]みたいなのを作るとループは減るか？
-- ちょっと手続きっぽいか？

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
        c2 = all (\y -> snd y == 2) r
        c3 = all ((/= 2) . length) $ (group . sort) $ map fst r
        r = rle x

-- |
-- >>> rle "aabb"
-- [('a',2),('b',2)]
rle :: String -> [(Char, Int)]
rle x = map (\y -> (head y, length y)) $ group x

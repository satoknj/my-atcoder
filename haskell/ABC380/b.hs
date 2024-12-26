import Data.List (group)

main :: IO ()
main = do
  line <- getLine
  putStrLn $ (unwords . map show . solve) line

-- | test
-- >>> solve "|---|-|----|-|-----|"
-- [3,1,4,1,5]
-- >>> solve "|----------|"
-- [10]
-- >>> solve "|-|-|-|------|"
-- [1,1,1,6]
solve :: String -> [Int]
solve = map length . filter (\x -> head x == '-') . group


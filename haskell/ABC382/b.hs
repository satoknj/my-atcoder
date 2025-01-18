import Data.List (elemIndices)

main :: IO ()
main = do
  nd <- getLine
  s <- getLine
  let [n,d] = (map read . words) nd
  putStrLn $ solve s n d

-- |
-- >>> solve ".@@.@" 5 2
-- ".@..."
solve :: String -> Int -> Int -> String
solve s n d = do
  let indiceis = elemIndices '@' s
  -- [1,2,4] -> [1]
  let a = last $ take (length indiceis - d) indiceis
  let b = a + 1
  take b s ++ replicate (n-b) '.'

import Data.List (elemIndices)

main :: IO ()
main = do
  nd <- getLine
  s <- getLine
  let [n,d] = (map read . words) nd
  let revS = reverse s
  let f = reverse . solve'' d
  putStrLn $ f revS

-- |
-- >>> solve'' 1 "@.@@."
-- "..@@."
solve'' :: Int -> String -> String
solve'' d [] = []
solve'' d ('.':xs) = '.' : solve'' d xs
solve'' 0 (x:xs) = x : solve'' 0 xs
solve'' d (x:xs) = '.' : solve'' (d-1) xs

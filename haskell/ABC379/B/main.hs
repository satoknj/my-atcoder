import Data.List (group)

main = do
  [n,k] <- map read . words <$> getLine
  s <- getLine

  print $ rle' n k s

-- | my rle
-- >>> rle' 7 3 "OOXOOOO"
-- 1
-- >>> rle' 12 2 "OXXOOOXOOOOX"
-- 3
rle' :: Int -> Int -> String -> Int
rle' n k s = do
  let a = [(head x, length x) | x <- group s]
  let b = filter (\x -> snd x >= k && fst x == 'O') a
  (sum . map ((`div` k) . snd)) b

import Data.List (group)

main = do
  [n,k] <- map read . words <$> getLine
  s <- getLine

  let a = [(head x, length x) | x <- group s]
  let b = filter (\x -> snd x >= k && fst x == 'O') a
  print $ (sum . map ((`div` k) . snd)) b

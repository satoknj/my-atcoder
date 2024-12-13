import Data.List (group)

main = do
  [n,k] <- map (read :: String -> Int) . words <$> getLine
  s <- getLine
  print n
  print k
  print s

  let a = [(head x, length x) | x <- group s]
  let b = filter (\x -> snd x > k && fst x == 'O') a
  print $ (sum . map ((`div` k) . snd)) b

  print "hoge"

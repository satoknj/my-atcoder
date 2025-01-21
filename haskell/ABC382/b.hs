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
-- >>> solve "@@@" 3 3
-- "..."
solve :: String -> Int -> Int -> String
solve s n d = 
  let indiceis = elemIndices '@' s
  in
    if null indiceis
    then
      fs
    else
      let a = take (length indiceis - d) indiceis
        in
          if null a
          then
            fs
          else
            let b = last a
              in
                let c = b + 1
                in
                  take c s ++ replicate (n-c) '.'
    where
      fs = replicate n '.'

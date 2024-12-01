import Control.Monad (replicateM)
import Data.Array (listArray, Array)

main :: IO ()
main = do
  n <- fmap read getLine :: IO Int
  qrs <- replicateM n getLine
  q <- fmap read getLine :: IO Int
  tds <- replicateM q getLine
  print n
  print qrs
  print tds

  let digits = map words qrs
  let nums = map (map read) digits
  let a = listArray (1, n) nums :: Array Int [Int]

  print $ tds !! 1
  print a


  print "done"

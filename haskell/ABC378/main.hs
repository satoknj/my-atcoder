import Control.Monad (replicateM)
import Data.Array (listArray, Array, (!))
import Data.Foldable (for_)

-- todo
-- 現状理解
-- タプルを使う

getInts :: IO [Int]
getInts =  map read.words <$> getLine

main :: IO ()
main = do
  -- get input
  n <- read <$> getLine
  qrs <- replicateM n getInts
  j <- read <$> getLine
  tds <- replicateM j getInts

  -- transform
  let qrArray = listArray (1, n) qrs :: Array Int [Int]

  -- solve
  for_ tds $ \td -> do
    let t = head td
    let d = td !! 1

    let qr = qrArray ! t :: [Int]
    let q = head qr
    let r = qr !! 1

    print $ d + (q - (d-r)) `mod` q

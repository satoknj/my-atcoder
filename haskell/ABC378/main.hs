import Control.Monad (replicateM)
import Data.Array (listArray, Array, (!))
import Data.Foldable (for_)

main :: IO ()
main = do
  -- get input
  n <- read <$> getLine
  qrs <- replicateM n getLine
  j <- read <$> getLine
  tds <- replicateM j getLine

  -- transform
  let digits = map words qrs
  let nums = map (map read) digits
  let qrArray = listArray (1, n) nums :: Array Int [Int]

  -- solve
  for_ tds $ \tdStr -> do
    let td = map read  (words tdStr) :: [Int]

    let t = head td
    let d = td !! 1
    let qr = qrArray ! t :: [Int]
    let q = head qr
    let r = qr !! 1

    print $ d + (q - (d-r)) `mod` q

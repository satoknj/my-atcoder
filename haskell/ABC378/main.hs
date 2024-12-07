import Control.Monad (replicateM)
import Data.Array (listArray, Array, (!))
import Data.Foldable (for_)

getIntTuple :: IO (Int, Int)
getIntTuple = (\[x,y] -> (x, y)).map read.words <$> getLine

main :: IO ()
main = do
  -- get input
  n <- read <$> getLine
  qrs <- replicateM n getIntTuple
  j <- read <$> getLine
  tds <- replicateM j getIntTuple

  -- transform
  let qrArray = listArray (1, n) qrs :: Array Int (Int, Int)

  -- solve
  for_ tds $ \(t, d) -> do
    let (q, r) = qrArray ! t :: (Int, Int)
    print $ d + (q - (d-r)) `mod` q

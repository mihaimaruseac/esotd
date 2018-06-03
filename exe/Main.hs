import Args (parseArgs)
import ExponentialSum (expSums)

main :: IO ()
main = do
  args <- parseArgs
  print $ args
  print $ take 10 $ expSums [1,2,3]

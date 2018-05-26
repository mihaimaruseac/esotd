import ExponentialSum (expSums)

main :: IO ()
main = print $ take 10 $ expSums [1,2,3]

module Main where
  
import AdventUtils
import Utils (readInt)

data Keys = MkKeys { _card :: Int, _door :: Int }
keys :: String -> Keys
keys input = case lines input of
  [c, d] -> MkKeys (readInt c) (readInt d)
  _ -> error $ "Malformed input: " <> input
  
main :: IO ()
main = do
  (MkKeys c d) <- keys <$> readInput "2020" "25"
  print c
  print d
  




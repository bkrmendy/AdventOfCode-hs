module Utils where

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- ^ https://stackoverflow.com/a/60380502
replace index elem = map (\(index', elem') -> if index' == index then elem else elem') . zip [0..]

concatRep :: Int -> String -> String
concatRep n = concat . replicate n


module Challenge where

class Challenge a where
  parse     :: String -> a
  partOne   :: a -> String
  partTwo   :: a -> String

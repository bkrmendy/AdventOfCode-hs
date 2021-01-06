{-# LANGUAGE FlexibleInstances #-}

module DayEleven where

import Challenge

data Element
  = Polonium
  | Thulium
  | Promethium
  | Ruthenium
  | Cobalt

data Component = M | G

floors :: [[(Component, Element)]]
floors = [ [(G, Polonium), (G, Thulium), (M, Thulium), (G, Promethium), (G, Ruthenium), (M, Ruthenium), (G, Cobalt), (M, Cobalt) ]
         , [(M, Polonium), (M, Promethium)]
         , []
         , []
         ]

instance Challenge [[(Component, Element)]] where
  parse _ = floors
  partOne = show . length
  partTwo = show . length

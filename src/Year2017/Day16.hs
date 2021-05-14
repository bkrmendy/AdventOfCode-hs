module Year2017.Day16 where
import            Challenge
import            Utils (int, parseLines)

import            Text.Parsec hiding (State)

import            Control.Monad.State
import            Data.List (find)
import            Data.Maybe (fromJust)
import qualified  Data.Array as A

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char

pSpin :: Parsec String () Move
pSpin = Spin <$> (char 's' *> int)

pExchange :: Parsec String () Move
pExchange = Exchange <$> (char 'x' *> int) <*> (char '/' *> int)

pPartner :: Parsec String () Move
pPartner = Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar)

pMove :: Parsec String () Move
pMove = pSpin <|> pExchange <|> pPartner

type Programs = A.Array Int Char

doMove :: Move -> State Programs ()
doMove (Spin n) = do
  modify' $ \ps -> A.array (A.bounds ps) [((i + n) `mod` 16, e) | (i, e) <- A.assocs ps]

doMove (Exchange ia ib) = do
  (ixa, a) <- gets $ fromJust . find ((==) ia . fst) . A.assocs
  (ixb, b) <- gets $ fromJust . find ((==) ib . fst) . A.assocs
  modify' $ \ps -> A.array (A.bounds ps) ([(ixa, b), (ixb, a)] <> [(i, e) | (i, e) <- A.assocs ps, i /= ia, i /= ib])

doMove (Partner a b) = do
  (ixa, ea) <- gets $ fromJust . find ((==) a . snd) . A.assocs
  (ixb, eb) <- gets $ fromJust . find ((==) b . snd) . A.assocs
  modify' $ \ps -> A.array (A.bounds ps) ([(ixa, eb), (ixb, ea)] <> [(i, e) | (i, e) <- A.assocs ps, e /= a, e /= b])

dance :: Int -> [Move] -> State Programs ()
dance n ms = do
  ps <- get
  per <- period ms ps
  put ps
  forM_ [1..(n `rem` per)] $ \_ -> do
    forM_ ms doMove


period :: [Move] -> Programs -> State Programs Int
period ms p = do
  forM_ ms doMove
  ps <- get
  if p == ps
    then return 1
    else (1 +) <$> period ms p


inOrder :: Programs -> String
inOrder = A.elems

instance Challenge [Move] where
  parse = parseLines (sepBy1 pMove (char ','))
  partOne ms = inOrder $ execState (dance 1 ms) (A.array (0, 15) $ zip [0..] ['a'..'p'])
  partTwo ms = inOrder $ execState (dance 1000000000 ms) (A.array (0, 15) $ zip [0..] ['a'..'p'])


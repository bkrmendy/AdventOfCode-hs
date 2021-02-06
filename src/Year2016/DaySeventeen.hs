module DaySeventeen where
import Prelude hiding (Left, Right)
import Challenge
import Data.Word8
import Data.List
import Data.Function
import qualified Data.ByteString as Bs
import qualified Data.ByteString.UTF8 as BLU
import Utils hiding (fst, snd)

newtype Passcode = Passcode { unPasscode :: Bs.ByteString }

data Direction = Up | Down | Left | Right deriving (Eq)

isOpen :: Word8 -> Bool
isOpen letter = Bs.count letter (BLU.fromString "bcdef") > 0

directions :: [Word8] -> [Direction]
directions = map fst . filter (isOpen . snd) . zip [Up, Down, Left, Right]

available :: (Int, Int) -> Direction -> Bool
available (row, column) = flip elem (availableVertical row ++ availableHorizontal column)
  where
    availableVertical 1 = [Down]
    availableVertical 4 = [Up]
    availableVertical _ = [Up, Down]
    availableHorizontal 1 = [Right]
    availableHorizontal 4 = [Left]
    availableHorizontal _ = [Left, Right]

fromDirection :: Direction -> Bs.ByteString
fromDirection = BLU.fromString . transfrom
  where
    transfrom Up    = "U"
    transfrom Down  = "D"
    transfrom Left  = "L"
    transfrom Right = "R"

move :: Direction -> (Int, Int) -> (Int, Int)
move Up (row, column) = (row - 1, column)
move Down (row, column) = (row + 1, column)
move Left (row, column) = (row, column - 1)
move Right (row, column) = (row, column + 1)

paths :: Passcode -> [Bs.ByteString]
paths (Passcode passcode) = search (1, 1) (Bs.pack [])
  where
    search :: (Int, Int) -> Bs.ByteString -> [Bs.ByteString]
    search position path
      | position == (4, 4) = [path]
      | otherwise =
        md5 (Bs.append passcode path) -- get hash from concatenated passcode and path
        & Bs.take 4 -- first 4 chars of hash
        & Bs.unpack -- unpack to list
        & directions -- get available directions
        & filter (available position) -- filter available direction based on position
        & continue position path -- continue search in available directions

    continue :: (Int, Int) -> Bs.ByteString -> [Direction] -> [Bs.ByteString]
    continue pos path = concatMap (uncurry search . transform)
      where
        transform :: Direction -> ((Int, Int), Bs.ByteString)
        transform dir = (move dir pos, Bs.append path (fromDirection dir))

instance Challenge Passcode where
  parse = Passcode . BLU.fromString
  partOne = show . head . sortOn Bs.length . paths
  partTwo = show . Bs.length . head . sortOn (negate . Bs.length) . paths
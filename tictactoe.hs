import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | X | B deriving (Eq, Ord, Show)

type Pos = (Int, Int)

initField :: Int -> Grid
initField n = replicate n (replicate n B)

next :: Player -> Player
next O = X
next X = O
next B = B


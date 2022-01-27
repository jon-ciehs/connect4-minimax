import Data.List (transpose)

-- BOARD

rows :: Int
rows = 6
cols :: Int
cols = 7
win :: Int
win = 4
depth :: Int
depth = 6

type Board = [Row]
type Row = [Player]

-- Best strategy: min for O and max for X as O < B < X from Ord deriving
data Player = O | B | X deriving (Ord, Eq, Show)

test :: Board
test = [[B,B,B,B,B,B,B],
        [B,B,B,B,B,B,B],
        [B,B,B,B,B,B,B],
        [B,B,B,X,X,B,B],
        [B,B,O,O,X,B,B],
        [B,O,O,X,X,X,O]]


-- Extract from the board candidate diagonals for the win, could be improved cause the Blank diagonals can't solve the game
diagonals :: Board -> [[Player]]
diagonals matrix = filter ((> 3) . length) $ lowerDiags matrix ++ upperDiags matrix ++ lowerDiags rev ++ upperDiags rev
    where lowerDiags = reverse . transpose . zipWith drop [1..]
          upperDiags = transpose . zipWith drop [0..] . transpose
          rev = reverse matrix

-- USER INTERFACE

showBoard :: Board -> IO ()
showBoard b =
  putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
  where
    showRow = map showPlayer
    line = replicate cols '-'
    nums = take cols ['0'..]

showPlayer :: Player -> Char
showPlayer O = 'O'
showPlayer B = '.'
showPlayer X = 'X'
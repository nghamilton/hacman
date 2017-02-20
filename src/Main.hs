{-# LANGUAGE EmptyDataDecls #-}


module Main where
import Data.Char
--import Data.Vector

data Item = W | D | S | P | E deriving (Show)

type Row = [Item]
type Board = [Row]
type Pos = (Int,Int)

board = [ [W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W],
  [W,D,D,D,D,D,D,D,D,D,D,D,D,W,W,D,D,D,D,D,D,D,D,D,D,D,D,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,S,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,S,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,W],
  [W,D,W,W,W,W,D,W,W,D,W,W,W,W,W,W,W,W,D,W,W,D,W,W,W,W,D,W],
  [W,D,D,D,D,D,D,W,W,D,D,D,D,W,W,D,D,D,D,W,W,D,D,D,D,D,D,W],
  [W,W,W,W,W,W,D,W,W,W,W,W,S,W,W,S,W,W,W,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,W,W,W,S,W,W,S,W,W,W,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,S,S,S,S,S,S,S,S,S,S,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,S,W,W,W,W,E,W,W,W,S,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,S,W,W,W,W,S,W,W,W,S,W,W,D,W,W,W,W,W,W],
  [S,S,S,S,S,S,D,S,S,S,W,E,S,E,S,E,S,W,S,S,S,D,S,S,S,S,S,S],
  [W,W,W,W,W,W,D,W,W,S,W,W,W,W,W,W,W,W,S,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,S,W,W,W,W,W,W,W,W,S,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,S,S,S,S,S,S,S,S,S,S,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,W,W,W,S,W,W,S,W,W,W,W,W,D,W,W,W,W,W,W],
  [W,W,W,W,W,W,D,W,W,W,W,W,S,W,W,S,W,W,W,W,W,D,W,W,W,W,W,W],
  [W,D,D,D,D,D,D,W,W,D,D,D,D,W,W,D,D,D,D,W,W,D,D,D,D,D,D,W],
  [W,D,W,W,W,W,D,W,W,D,W,W,W,W,W,W,W,W,D,W,W,D,W,W,W,W,D,W],
  [W,D,D,D,D,D,D,D,D,D,D,D,D,S,D,D,D,D,D,D,D,D,D,D,D,D,D,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,S,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,S,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,D,D,D,D,D,D,D,D,D,D,D,D,W,W,D,D,D,D,D,D,D,D,D,D,D,D,W],
  [W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W] ] :: Board

startPos = (14,21)

main :: IO ()
main = do
  updateBoard board startPos

updateBoard :: Board -> Pos -> IO ()
updateBoard b p = do
-- take one char input
--  print p
  putStrLn $ replicate 100 '\n' 
  putStrLn $ showBoard b p
  c <- getChar
-- validate move (ie no wall)
  case validate c of
    Nothing -> main
    Just offset -> do
      let p'@(x,y) = updatePos p offset 
      let b' = eatDot b p
      case (board!!y!!x) of
        W -> updateBoard b p 
        E -> undefined 
        _ -> updateBoard b' p' 
--
-- refresh board state
-- show board
-- rinse, repeat
-- putSteupdateBoard b p Lce n item ls = a ++ (item:b) where (a, (_:b)) =
-- splitAt n lsn $ showBoard board


updatePos (x,y) (x',y') = (x+x',y+y')

validate 'u' = Just (0,-1)
validate 'h' = Just (-1,0)
validate 'k' = Just (1,0)
validate 'j' = Just (0,1)
validate _ = Just (0,0) 

showBoard :: Board -> Pos -> String
showBoard b (x,y) = concat $ map showRow $ b' where
  b' = upd y x P b
  upd row col x xs =
      let row_to_replace_in = xs !! row
          modified_row = replace col x row_to_replace_in
      in replace row modified_row xs
  replace n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

eatDot :: Board -> Pos -> Board 
eatDot b (x,y) = b' where
  b' = upd y x S b
  upd row col x xs =
      let row_to_replace_in = xs !! row
          modified_row = replace col x row_to_replace_in
      in replace row modified_row xs
  replace n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

showRow :: Row -> String
showRow r = map conv r ++ ['\n'] where
  conv W = '#' -- 219 
  conv D = '.' -- 167
  conv S = ' '
  conv P = '©' -- 184
  conv E = 'X'

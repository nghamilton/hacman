{-# LANGUAGE EmptyDataDecls #-}


module Hackman where
import Data.Char
--import Data.Vector

data Item = W | D | S | P | E deriving (Show, Eq)

type Row = [Item]
type Board = [Row]
type Pos = (Int,Int)
type Score = Int

board = [ [W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W],
  [W,D,D,D,D,D,D,D,D,D,D,D,D,W,W,D,D,D,D,D,D,D,D,D,D,D,D,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
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
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,D,W,W,W,W,D,W,W,W,W,W,D,W,W,D,W,W,W,W,W,D,W,W,W,W,D,W],
  [W,D,D,D,D,D,D,D,D,D,D,D,D,W,W,D,D,D,D,D,D,D,D,D,D,D,D,W],
  [W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W] ] :: Board

startPos = (13,21)

main :: IO ()
main = do
  loopBoard (board,startPos,0)

esC a = '\27':a
col c a = esC c ++ a ++ esC "[0m" 
blue = "[94m"
blueb = "[44m"
yellowb = "[33m"


loopBoard :: (Board,Pos,Score) -> IO ()
loopBoard (b,p,s) = do
  putStrLn $ esC "[2J"
  putStrLn $ show s
  putStrLn $ showBoard $ addPacman b p 
  c <- getChar
  loopBoard $ updateBoard b p c s

updateBoard :: Board -> Pos -> Char -> Score -> (Board,Pos,Score)
updateBoard b p c s =
  case validate c of
    Nothing -> (b,p,s)
    Just offset -> res where
      p'@(x,y) = updatePos p offset 
      res = case (board!!y!!x) of
        W -> (b,p,s) 
        _ -> (b',p',s+s')
      (b',s') = eat b p'

updatePos :: Pos -> Pos -> Pos
updatePos (x,y) (x',y') =  (doWrap(x+x'), doWrap(y+y'))

doWrap :: Int -> Int
doWrap -1 = 27
doWrap 28 = 0
doWrap i = i

validate 'u' = Just (0,-1)
validate 'h' = Just (-1,0)
validate 'k' = Just (1,0)
validate 'j' = Just (0,1)
validate _ = Nothing

editBoard :: Board -> Item -> Pos -> Board 
editBoard b i (x,y) = b' where
  b' = upd y x i b
  upd row col x xs =
      let row_to_replace_in = xs !! row
          modified_row = replace col x row_to_replace_in
      in replace row modified_row xs
  replace n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

eat :: Board -> Pos -> (Board,Score)
eat b (x,y) = (b',s) where
  b' = editBoard b S (x,y)
  s = case b!!y!!x of
    D -> 1
    E -> 100
    _ -> 0

addPacman :: Board -> Pos -> Board 
addPacman b (x,y) = editBoard b P (x,y)

showBoard :: Board -> String
showBoard b = concat $ map showRow b 

showRow :: Row -> String
showRow r = concat $ map conv r ++ ["\n"] where
  conv W = col blueb " "
  conv D = "."
  conv S = " "
  conv P = col yellowb "©" 
  conv E = "X"

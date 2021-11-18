{-# language TupleSections #-}

import Data.Maybe
import Data.List
import Control.Monad
import Data.String.Utils
import Criterion.Main

data SpotFill = Unknown | No | Yes deriving Show
type Board = [[SpotFill]]
type Constrs = [[Int]]

boardBad :: Board -> Constrs -> Constrs -> Bool
boardBad board rowConstrs colConstrs = rowsBad (zip board rowConstrs) || rowsBad (zip (transpose board) colConstrs)

rowsBad :: [([SpotFill],[Int])] -> Bool
rowsBad board = or $ uncurry rowBad <$> board

rowBad :: [SpotFill] -> [Int] -> Bool
rowBad [] [] = False
rowBad (Unknown:_) _ = False
rowBad (Yes:fr) (n:nr) = rowBadT fr (n-1) nr
rowBad (No:fr) ns = rowBad fr ns
rowBad _ _ = True

rowBadT :: [SpotFill] -> Int -> [Int] -> Bool
rowBadT [] 0 ns = rowBad [] ns
rowBadT (Unknown:_) _ _ = False
rowBadT (No:r) 0 ns = rowBad r ns
rowBadT (Yes:r) n ns = rowBadT r (n-1) ns
rowBadT _ _ _ = True

fillSpot :: SpotFill -> (Int, Int) -> Board -> Board
fillSpot v (n, 0) (h:t) = (fillSpotRow v n h):t
fillSpot v (n, m) (h:t) = h:(fillSpot v (n, m-1) t)

fillSpotRow :: SpotFill -> Int -> [SpotFill] -> [SpotFill]
fillSpotRow v 0 (_:t) = v:t
fillSpotRow v n (h:t) = h:(fillSpotRow v (n-1) t)

emptySpot :: Board -> Maybe (Int, Int) -- col num, row num
emptySpot [] = Nothing
emptySpot (row:rows) = maybe (((+ 1) <$>) <$> emptySpot rows) (pure . (,0)) $ emptySpotRow row

emptySpotRow :: [SpotFill] -> Maybe Int
emptySpotRow [] = Nothing
emptySpotRow (Unknown:_) = pure 0
emptySpotRow (_:r) = (+ 1) <$> emptySpotRow r

solveBoard :: Board -> Constrs -> Constrs -> Maybe Board
solveBoard board rowConstrs colConstrs = maybe (pure board) trySolve $ emptySpot board where
  trySolve :: (Int, Int) -> Maybe Board
  trySolve i = maybe (trySolveV Yes i) pure $ trySolveV No i

  trySolveV :: SpotFill -> (Int, Int) -> Maybe Board
  trySolveV v i = if (not $ boardBad spotFilled rowConstrs colConstrs) then (solveBoard spotFilled rowConstrs colConstrs) else Nothing where spotFilled = fillSpot v i board

boardOfSize :: Int -> Int -> Board
boardOfSize n m = replicate n $ replicate n Unknown

solveBoardOfSize :: Int -> Int -> Constrs -> Constrs -> Maybe Board
solveBoardOfSize n m = solveBoard $ boardOfSize n m

prettyPrintBoard :: Board -> String
prettyPrintBoard = _prettyPrintBoard 5

_prettyPrintBoard :: Int -> Board -> String
_prettyPrintBoard _ [] = ""
_prettyPrintBoard 0 l@(h:t) = (replicate (length s) '-') <> "\n" <> s <> "\n" <> _prettyPrintBoard 4 t where s = _prettyPrintBoardRow 5 h
_prettyPrintBoard n (h:t) = _prettyPrintBoardRow 5 h <> "\n" <> _prettyPrintBoard (n-1) t

_prettyPrintBoardRow :: Int -> [SpotFill] -> String
_prettyPrintBoardRow _ [] = ""
_prettyPrintBoardRow 0 l = '|' : (_prettyPrintBoardRow 5 l)
_prettyPrintBoardRow n (h:t) = prettyPrintSpotFill h <> _prettyPrintBoardRow (n-1) t

prettyPrintSpotFill :: SpotFill -> String
prettyPrintSpotFill Yes = "#"
prettyPrintSpotFill No = "."
prettyPrintSpotFill Unknown = "?"

inputConstrs :: IO [Int]
inputConstrs = do
  s <- replace " " "," <$> getLine
  pure $ read $ "[" <> s <> "]"

main = do
  putStrLn "enter n:"
  n <- readLn
  putStrLn "enter m:"
  m <- readLn
  putStrLn "enter row constraints:"
  rowConstrs <- replicateM n inputConstrs
  putStrLn "enter col constraints:"
  colConstrs <- replicateM m inputConstrs
  putStrLn $ maybe "cannot solve" (("your solved board:\n" <>) . prettyPrintBoard) $ solveBoardOfSize n m rowConstrs colConstrs
  defaultMain [ bgroup "solve" [ bench "solve" $ whnf (\(a,b,c,d) -> solveBoardOfSize a b c d) (n,m,rowConstrs,colConstrs) ] ]

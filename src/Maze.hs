{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Maze (Maze, rectangularMaze, symmetric, maze, cells, walls, toList, breakWall, fromList, farthestCell) where

-- based on https://gist.github.com/jamis/f5921c1037079c72083e
-- by Jamis Buck (jamis@jamisbuck.org)

import Control.Monad.State
import System.Random

import qualified Data.Map as M
import qualified Data.Set as S

data Maze a = Maze (M.Map a (S.Set a)) deriving (Show)

type RandomM a = State StdGen a

cells :: Ord a => Maze a -> [a]
cells (Maze m) = M.keys m

walls :: Ord a => Maze a -> a -> [a]
walls (Maze m) x = S.toList (m M.! x)

fromList :: Ord a => [(a, [a])] -> Maze a
fromList xs = Maze $ M.fromList $ map (\(x,y) -> (x, S.fromList y)) xs

toList :: Ord a => Maze a -> [(a, [a])] 
toList (Maze m) = map (\(x,y) -> (x, S.toList y)) $ M.toList m

randElem :: S.Set a -> RandomM a
randElem s = do
  g <- get
  let (n, g') = randomR (0, S.size s - 1) g
  put g'
  return $ (S.toList s) !! n

farthestCell' :: Ord a => Maze a -> Maze a -> a -> (Int,a)
farthestCell' (Maze ns) (Maze ws) start =
  let paths = S.toList $ S.difference (ns M.! start) (ws M.! start)
      (d,x) = maximum $ (map f paths) ++ [(0,start)] in
    (d+1,x)
  where f p = farthestCell' (Maze ns) (addWall (Maze ws) start p) p

farthestCell :: Ord a => Maze a -> Maze a -> a -> a
farthestCell n w s = snd $ farthestCell' n w s

rectangularMaze :: Int -> Int -> Maze (Int,Int)
rectangularMaze width height = Maze $ M.fromList nodes
  where nodes = [ ((x,y), neighbors x y) |
                  x <- [0..width-1], y <- [0..height-1] ]
        neighbors x y =
          S.fromList $
            [ (nx, y) | nx <- [x+1,x-1], nx >= 0, nx < width ] ++
            [ ( x,ny) | ny <- [y+1,y-1], ny >= 0, ny < height ]

symmetric :: Ord a => Maze a -> Bool
symmetric (Maze m) = M.foldrWithKey f True m
  where f cell neighbors b = S.foldr' (g cell) b neighbors
        g cell neighbor b = b && S.member cell (m M.! neighbor) 

addWall :: Ord a => Maze a -> a -> a -> Maze a
addWall (Maze m) a b = Maze $ M.alter (with b) a $ M.alter (with a) b $ m
  where with :: Ord a => a -> Maybe (S.Set a) -> Maybe (S.Set a)
        with _ Nothing = Nothing
        with x (Just y) = Just $ S.insert x y

breakWall :: Ord a => Maze a -> a -> a -> Maze a
breakWall (Maze m) a b = Maze $ M.alter (without b) a $ M.alter (without a) b $ m
  where without :: Ord a => a -> Maybe (S.Set a) -> Maybe (S.Set a)
        without _ Nothing = Nothing
        without x (Just y) = Just $ S.delete x y
  
-- Generate a maze using the given random generator. 'count' is the number of
-- cells that are still unvisited. 'maze' is the current state of the maze,
-- 'pos' is the current position in the maze, and 'dirs' is an array of
-- directions that have not been tried from the current position.
--
--          count -> maze -> position -> visited -> neighbors 
generate :: Ord a => Int -> Maze a -> a -> S.Set a -> S.Set a -> RandomM (Maze a)

-- if there are no more cells that need to be visited, we're done
generate 0 m _ _ _ = return m

-- if there are no more directions that we can try, we need to choose a new
-- (visited) point and start fresh from there
generate count (Maze m) _ visited neighbors | S.null neighbors = do
  x <- randElem visited
  generate count (Maze m) x visited (m M.! x) 
  
-- otherwise, choose a new direction from the list of untried directions.
-- If we can, move in that direction, otherwise recurse and try another
-- direction
generate count (Maze m) position visited neighbors = do
  n <- randElem neighbors
  if S.member n visited
    -- can't move that way, so we try again
    then generate count (Maze m) position visited (S.delete n neighbors)
    -- move in the given direction
    else let Maze m' = breakWall (Maze m) position n in
         generate (count - 1) (Maze m') n (S.insert n visited)
                  (m' M.! n) 
  
maze :: Ord a => StdGen -> Maze a -> a -> Maze a
maze gen (Maze m) startingPosition =
  let count = length $ M.keys m
      visited = S.singleton startingPosition
      neighbors = m M.! startingPosition
  in
    evalState (generate (count - 1) (Maze m) startingPosition visited neighbors) gen

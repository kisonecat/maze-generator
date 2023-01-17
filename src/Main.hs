{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Graphics.PDF
import Control.Monad
import Data.Text (pack, Text)

import System.Random

import Maze

paperWidth :: Double 
paperWidth = 8.5 * 72

paperHeight :: Double 
paperHeight = 11 * 72

mazeWidth = 12
mazeHeight = 6

data Label = Label
    { _x :: Int
    , _y :: Int
    , _front :: Bool
    } deriving (Ord, Eq)

instance Show Label where
  show (Label { _x = x, _y = y, _front = True} )  = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Label { _x = x, _y = y, _front = False} )  = "<<" ++ (show x) ++ "," ++ (show y) ++ ">>"

front :: Int -> Int -> Label
front x y = Label { _x = x, _y = y, _front = True }

back :: Int -> Int -> Label
back x y = Label { _x = x, _y = y, _front = False }
 
range :: Int -> [Int]
range n = [0..n-1]

piece :: Bool -> Maze Label
piece s = cut $ fromList nodes
  where side = if s then front else back
        opposite = if s then back else front
        nodes = [ (side x y, neighbors x y) | x <- range (2*mazeWidth),
                                              y <- range (4*mazeHeight),
                                              y < 3*mazeHeight || x >= mazeWidth]
        neighbors x y = [ side nx y | nx <- [x+1,x-1], nx >= 0, nx < (2*mazeWidth), y < 3*mazeHeight || nx >= mazeWidth ] ++
                        [ side x ny | ny <- [y+1,y-1], ny >= 0, ny < (4*mazeHeight), ny < 3*mazeHeight || x >= mazeWidth ] ++
                        [ opposite x (6*mazeHeight - 1 - y) | x == 2*mazeWidth - 1, y >= 2*mazeHeight ]
        cut maze = foldl f maze [mazeWidth .. 2*mazeWidth - 1]
        f maze x = breakWall maze (side x (2*mazeHeight-1)) (side x (2*mazeHeight )) 

bothSides :: Maze Label
bothSides = fromList $ (toList (piece True)) ++ (toList (piece False))

drawStart :: PDFFloat -> PDFFloat -> Draw ()
drawStart x y = do
  fill $ Circle x y (0.25)

drawArrow:: PDFFloat -> PDFFloat -> Draw ()
drawArrow x y = do
  let cx = x + 0.5
  let cy = y + 0.5
  let a = 0.1
  let b = 0.4
  withNewContext $ do
    setStrokeAlpha 0.35
    stroke $ Line cx cy (cx + b) cy
    stroke $ Line (cx + b) cy (cx + b - a) (cy - a)
    stroke $ Line (cx + b) cy (cx + b - a) (cy + a)

drawEnd :: PDFFloat -> PDFFloat -> Draw ()
drawEnd x y = do
  withNewContext $ do
    applyMatrix $ translate (x :+ y)
    applyMatrix $ rotate $ Degree (-18)

    let radius = 0.35
    let radius' = 0.12

    beginPath (radius' :+ 0)
    forM_ [2,4,6,8,10] $ \t -> do
      let angle = Degree (36 * t)
      let angle' = Degree (36 * (t+1))
      lineto $ transform (rotate angle) (radius :+ 0)
      lineto $ transform (rotate angle') (radius' :+ 0)
      
    closePath
    fillPath

createPageContent :: Maze Label -> Bool -> Label -> Label -> PDFReference PDFPage -> PDF ()
createPageContent m side starting ending page = drawWithPage page $ do
  withNewContext $ do
    applyMatrix $ translate $ (paperWidth / 2.0) :+ (paperHeight / 2.0)

    let sx = _x starting
    let sy = _y starting
    let ex = _x ending
    let ey = _y ending
    let s = 20.0

    let width = fromIntegral mazeWidth
    let height = fromIntegral mazeHeight

    applyMatrix $ scale s s
    setWidth $ 1.0 / s

    applyMatrix $ translate $ (-width) :+ (-3*height)

    when (side == False) $ do
      applyMatrix $ translate $ (2*width) :+ (6*height)
      applyMatrix $ scale (-1) (-1)

    let theseCells = filter (\x -> _front x == side) $ cells m

    setLineCap RoundCap

    withNewContext $ do
      setWidth $ 4.0 / s
      stroke $ Line 0 0 (2*width) 0
      stroke $ Line (2*width) 0 (2*width) (2*height)
      stroke $ Line 0 0 0 (3*height)
      stroke $ Line width (3*height) width (4*height)

      stroke $ Line 0 (3*height) 0 (6*height)
      stroke $ Line 0 (6*height) (2*width) (6*height)
      stroke $ Line (2*width) (6*height) (2*width) (4*height)

    withNewContext $ do
      setWidth $ 4.0 / s
      --setDash $ DashPattern [4.0/s, 4.0/s] 0
      stroke $ Line 0 (3*height) width (3*height)
      stroke $ Line width (2*height) (2*width) (2*height)
      stroke $ Line width (4*height) (2*width) (4*height)

    when (side == _front starting) $ drawStart (fromIntegral sx + 0.5) (fromIntegral sy + 0.5)
    when (side == _front ending) $ drawEnd (fromIntegral ex + 0.5) (fromIntegral ey + 0.5)
    
    forM_ theseCells $ \c -> do
      let (x,y) = (_x c, _y c)
      let x' = fromIntegral x
      let y' = fromIntegral y

      when (x == (2*mazeWidth - 1) && y >= 2*mazeHeight && (all (\w -> _front w == side) $ walls m c)) $ drawArrow x' y'  
      
      forM_ (walls m c) $ \w -> do
        let (wx,wy) = (_x w, _y w)
        let wx' = fromIntegral wx
        let wy' = fromIntegral wy 

        let dx = wx - x
        let dy = wy - y

        when (_front w == side) $ do
          when (dx ==  0 && dy ==  1) $ stroke $ Line x' (y'+1) (x'+1) (y'+1)
          when (dx ==  1 && dy ==  0) $ stroke $ Line (x'+1) y' (x'+1) (y'+1)
          when (dx ==  0 && dy == -1) $ stroke $ Line x' y' (x'+1) y'
          when (dx == -1 && dy ==  0) $ stroke $ Line x' y' x' (y'+1)

        withNewContext $ do
          --setLineCap ButtCap
          setWidth $ 4.0 / s
          when (_front w /= side && x == (2*mazeWidth - 1)) $ stroke $ Line (x'+1) y' (x'+1) (y'+1)

myFrontDocument :: Maze Label -> Label -> Label -> PDF ()
myFrontDocument m starting ending = do
    page1 <- addPage Nothing
    createPageContent m True starting ending page1
    page2 <- addPage Nothing
    createPageContent m False starting ending page2

main :: IO()
main = do
    let rect = PDFRect 0 0 paperWidth paperHeight
    let m = bothSides

    g <- getStdGen
    let startingPosition = front 0 0
    print m 
    let m' = maze g m startingPosition
    print m'

    let endingPosition = farthestCell m m' startingPosition
    
    print $ cells m'
    print $ symmetric m
    print $ symmetric m'
    
    Right helveticaBold <- mkStdFont Helvetica_Bold
    Right courier <- mkStdFont Courier  

    runPdf "maze.pdf" (standardDocInfo { author="Jim Fowler", compressed = False}) rect $ do
        myFrontDocument m' startingPosition endingPosition

  

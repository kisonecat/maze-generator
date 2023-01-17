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

mazeWidth = 20
mazeHeight = 18

data Label = Label
    { _x :: Int
    , _y :: Int
    } deriving (Ord, Eq)

instance Show Label where
  show (Label { _x = x, _y = y} )  = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
  show (Label { _x = x, _y = y} )  = "<<" ++ (show x) ++ "," ++ (show y) ++ ">>"

front :: Int -> Int -> Label
front x y = Label { _x = x, _y = y }

range :: Int -> [Int]
range n = [0..n-1]

piece :: Maze Label
piece = fromList nodes
  where nodes = [ (front x y, neighbors x y) | x <- range mazeWidth,
                                              y <- range mazeHeight ]
        neighbors x y = [ front ((nx + mazeWidth) `mod` mazeWidth) y | nx <- [x+1,x-1] ] ++
                        [ front x ((ny + mazeHeight) `mod` mazeHeight) | ny <- [y+1,y-1] ]

bothSides :: Maze Label
bothSides = piece 

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

createPageContent :: Maze Label -> Label -> Label -> PDFReference PDFPage -> PDF ()
createPageContent m starting ending page = drawWithPage page $ do
  withNewContext $ do
    applyMatrix $ translate $ (paperWidth / 2.0) :+ (paperHeight / 2.0)
    
    let sx = _x starting
    let sy = _y starting
    let ex = _x ending
    let ey = _y ending
    let s = 18.0

    let width = fromIntegral mazeWidth
    let height = fromIntegral mazeHeight

    applyMatrix $ scale s s
    setWidth $ 1.0 / s

    applyMatrix $ translate $ (-width/2) :+ (-height/2)

    setLineCap RoundCap

    drawStart (fromIntegral sx + 0.5) (fromIntegral sy + 0.5)
    drawEnd (fromIntegral ex + 0.5) (fromIntegral ey + 0.5)
    
    forM_ (cells m) $ \c -> do
      let (x,y) = (_x c, _y c)
      let x' = fromIntegral x
      let y' = fromIntegral y

      forM_ (walls m c) $ \w -> do
        let (wx,wy) = (_x w, _y w)
        let wx' = fromIntegral wx
        let wy' = fromIntegral wy 

        let dx = wx - x
        let dy = wy - y

        when (dx ==  0 && dy ==  1) $ stroke $ Line x' (y'+1) (x'+1) (y'+1)
        when (dx ==  1 && dy ==  0) $ stroke $ Line (x'+1) y' (x'+1) (y'+1)
        when (dx ==  0 && dy == -1) $ stroke $ Line x' y' (x'+1) y'
        when (dx == -1 && dy ==  0) $ stroke $ Line x' y' x' (y'+1)

        withNewContext $ do
          setWidth $ 4.0 / s
          when (dx ==  0 && dy <  -1) $ stroke $ Line x' (y'+1) (x'+1) (y'+1)
          when (dx <  -1 && dy ==  0) $ stroke $ Line (x'+1) y' (x'+1) (y'+1)
          when (dx ==  0 && dy >   1) $ stroke $ Line x' y' (x'+1) y'
          when (dx >   1 && dy ==  0) $ stroke $ Line x' y' x' (y'+1)

myFrontDocument :: Maze Label -> Label -> Label -> PDF ()
myFrontDocument m starting ending = do
    page1 <- addPage Nothing
    createPageContent m starting ending page1

main :: IO()
main = do
    let rect = PDFRect 0 0 paperWidth paperHeight
    let m = bothSides

    g <- getStdGen
    let startingPosition = front 2 5
    let m' = maze g m startingPosition

    let endingPosition = farthestCell m m' startingPosition
    
    print $ cells m'
    print $ symmetric m
    print $ symmetric m'
    
    Right helveticaBold <- mkStdFont Helvetica_Bold
    Right courier <- mkStdFont Courier  

    runPdf "torus.pdf" (standardDocInfo { author="Jim Fowler", compressed = False}) rect $ do
        myFrontDocument m' startingPosition endingPosition

  

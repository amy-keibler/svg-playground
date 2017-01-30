{-# LANGUAGE OverloadedStrings #-}
module Main where

import Animation

import Graphics.Svg hiding (translate)
import Data.Text (pack)
import System.IO

frameWidth :: Int
frameWidth = 64

numFrames :: Int
numFrames = 5

frameLines :: Int -> Line -> Line -> (Line, Line)
frameLines index line0 line1 = (translate frameStart line0, translate frameStart line1)
  where frameStart = (Point (index * frameWidth) 0)

framedLinesToSvg :: (Line, Line) -> Element
framedLinesToSvg (line0, line1) = (lineToSvg line0) <> (lineToSvg line1)

main :: IO ()
main = do
  let line0 = linearAnimateLine numFrames (Line (Point 20 20) (Point 44 44)) (Line (Point 20 20) (Point 20 44))
  let line1 = linearAnimateLine numFrames (Line (Point 20 44) (Point 44 20)) (Line (Point 44 20) (Point 44 44))
  let lineFrames = foldr1 (<>) $ fmap framedLinesToSvg $ zipWith3 frameLines [0 ..] line0 line1
  let file = svg (frames <> lineFrames)
  print file
  writeFile "/tmp/spritesheet.svg" $ show file

svg :: Element -> Element
svg content = doctype <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- (pack $ show $ (frameWidth * numFrames)), Height_ <<- "64"]

defaultFrame :: Int -> Element
defaultFrame index = rect_ [X_ <<- ((pack . show) (index * frameWidth)), Y_ <<- "0",
                            Width_ <<- "64", Height_ <<- "64", "red" ->> Stroke_]

frames :: Element
frames = foldr1 (<>) $ fmap defaultFrame [0 .. numFrames]

lineToSvg :: Line -> Element
lineToSvg (Line (Point xS yS) (Point xE yE)) = line_ [X1_ <<- (pack $ show xS), Y1_ <<- (pack $ show yS),
                                                      X2_ <<- (pack $ show xS), Y2_ <<- (pack $ show yS),
                                                      Stroke_ <<- "red", Stroke_width_ <<- "4"]

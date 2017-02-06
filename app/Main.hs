{-# LANGUAGE OverloadedStrings #-}
module Main where

import Animation

import Graphics.Svg hiding (translate)
import Data.Text hiding (foldr1, index, zip, map)

import Data.List (zip4)

frameWidth :: Int
frameWidth = 64

numFrames :: Int
numFrames = 10

framedLinesToSvg :: (Line, Line, Line, Line) -> Element
framedLinesToSvg (line0, line1, line2, line3) = thickLineSvg line0
                                                <> thickLineSvg line1
                                                <> thinLineSvg line2
                                                <> thinLineSvg line3
  where thickLineSvg = lineToSvg "8" "#990000"
        thinLineSvg = lineToSvg "4" "#cc0000"

main :: IO ()
main = do
  let topBackSlash = linearAnimateLine numFrames (Line (Point 20 20) (Point 44 44)) (Line (Point 26 20) (Point 26 44))
  let topForwardSlash = linearAnimateLine numFrames (Line (Point 20 44) (Point 44 20)) (Line (Point 38 44) (Point 38 20))
  let bottomBackSlash = linearAnimateLine numFrames (Line (Point 16 16) (Point 48 48)) (Line (Point 26 18) (Point 26 46))
  let bottomForwardSlash = linearAnimateLine numFrames (Line (Point 16 48) (Point 48 16)) (Line (Point 38 46) (Point 38 18))
  let lineFrames = fmap (show . svg . (defaultFrame <> )) $ framedLinesToSvg <$> zip4 bottomBackSlash bottomForwardSlash topBackSlash topForwardSlash
  let indexedFiles = zip [0..] lineFrames
  mapM_ (\(index, file) -> writeFile ("/tmp/spritesheet_" ++ (show index) ++ ".svg") file) indexedFiles

svg :: Element -> Element
svg content = doctype <> with (svg11_ content) [Version_ <<- "1.1",
                                                Width_ <<- frameSize,
                                                Height_ <<- frameSize]
              where frameSize = pack $ show frameWidth

defaultFrame :: Element
defaultFrame = rect_ [X_ <<- "2", Y_ <<- "2",
                            Width_ <<- paddedFrame 2, Height_ <<- paddedFrame 2,
                            "#660000" ->> Fill_, Fill_opacity_ <<- "0.5"]
                     <> rect_ [X_ <<- "4", Y_ <<- "4",
                               Width_ <<- paddedFrame 4, Height_ <<- paddedFrame 4,
                               "#990000" ->> Stroke_, Stroke_width_ <<- "8",
                               Fill_opacity_ <<- "0"]
                     <> rect_ [X_ <<- "2", Y_ <<- "2",
                               Width_ <<- paddedFrame 2, Height_ <<- paddedFrame 2,
                               "#cc0000" ->> Stroke_, Stroke_width_ <<- "4",
                               Fill_opacity_ <<- "0"]
  where paddedFrame padding = (pack . show) (frameWidth - (2 * padding))

lineToSvg :: Text -> Text -> Line -> Element
lineToSvg width color(Line (Point xS yS) (Point xE yE)) = line_ [X1_ <<- pack (show xS), Y1_ <<- pack (show yS),
                                                      X2_ <<- pack (show xE), Y2_ <<- pack (show yE),
                                                      Stroke_ <<- color, Stroke_width_ <<- width]

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Animation
import AnimationConfig

import Graphics.Svg hiding (translate)
import Data.Text (pack)
import System.Environment (getArgs)

import Data.List (transpose)

framedLinesToSvg :: [(Line, SVGStyling)] -> Element
framedLinesToSvg lines = foldr1 (<>) (uncurry lineToSvg <$> lines)

filenameArg :: [String] -> String
filenameArg [] = undefined
filenameArg (filename':_) = filename'

main :: IO ()
main = getArgs >>= handleFileName
  where handleFileName (filepath:_) = loadConfig <$> readFile filepath >>= outputAnimiation
        handleFileName _ = putStrLn "Please pass the filename of the json config file."

outputAnimiation :: Either String AnimationConfig -> IO ()
outputAnimiation (Left err) = putStrLn err
outputAnimiation (Right config) = do
  let numF = numFrames config
  let lineFrames = fmap (show . svg (frameWidth config, frameHeight config) . ((configFrame $ staticShapes config) <> )) $ framedLinesToSvg <$> transpose (zipConfigs numF (animatedShapes config))
  let indexedFiles = zip [0..] lineFrames
  mapM_ (\(index, file) -> writeFile (filename config ++ show (index :: Integer) ++ ".svg") file) indexedFiles

svg :: (Int, Int) -> Element -> Element
svg (frameWidth', frameHeight') content = doctype <> with (svg11_ content) [Version_ <<- "1.1",
                                                Width_ <<- frameW,
                                                Height_ <<- frameH]
              where frameW = pack $ show frameWidth'
                    frameH = pack $ show frameHeight'

configFrame :: [SVGShape] -> Element
configFrame = foldr (<>) mempty . fmap rectangleToElement

rectangleToElement :: SVGShape -> Element
rectangleToElement (SVGRect rect style) = rect_ [X_ <<- (pack . show . x . topLeft) rect,
                                                  Y_ <<- (pack . show . y . topLeft) rect,
                                                  Width_ <<- (pack . show $ ((x . bottomRight) rect - (x . topLeft) rect)),
                                                  Height_ <<- (pack . show $ ((y . bottomRight) rect - (y . topLeft) rect)),
                                                  Stroke_ <<- (pack . stroke) style,
                                                  Stroke_width_ <<- (pack . strokeWidth) style,
                                                  Fill_ <<- (pack . fill) style,
                                                  Fill_opacity_ <<- (pack . fillOpacity) style]

lineToSvg :: Line -> SVGStyling -> Element
lineToSvg (Line (Point xS yS) (Point xE yE)) (SVGStyling color width _ _) = line_ [X1_ <<- pack (show xS), Y1_ <<- pack (show yS),
                                                      X2_ <<- pack (show xE), Y2_ <<- pack (show yE),
                                                      Stroke_ <<- pack color, Stroke_width_ <<- pack width]

zipConfigs :: Int -> [SVGAnimation] -> [[(Line, SVGStyling)]]
zipConfigs numFrames = fmap (\(SVGAnimationLine start end style) -> zip (animate numFrames start end) (repeat style))

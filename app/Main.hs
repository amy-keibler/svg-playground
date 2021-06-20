{-# LANGUAGE OverloadedStrings #-}
module Main where

import AnimationConfig
import SVG

import Graphics.Svg hiding (translate, toElement)
import Data.Text (pack)
import System.Environment (getArgs)


main :: IO ()
main = getArgs >>= handleFileName
  where handleFileName (filepath:_) = readFile filepath >>= outputAnimiation . loadConfig
        handleFileName _ = putStrLn "Please pass the filename of the json config file."

outputAnimiation :: Either String AnimationConfig -> IO ()
outputAnimiation (Left err) = putStrLn err
outputAnimiation (Right config) = mapM_ (\(index, file) -> writeFile (filename config ++ show (index :: Int) ++ ".svg") file) $ files config

svg :: AnimationConfig -> Element -> Element
svg config content = doctype <> with (svg11_ content) [Version_ <<- "1.1",
                                                       Width_ <<- frameW,
                                                       Height_ <<- frameH]
              where frameW = pack $ show $ frameWidth config
                    frameH = pack $ show $ frameHeight config

staticSVGPart :: AnimationConfig -> Element
staticSVGPart = mconcat . fmap toElement . staticShapes

dynamicSVGPart :: AnimationConfig -> [Element]
dynamicSVGPart config = animationsToElelemnts (numFrames config) (animatedShapes config)

files :: AnimationConfig -> [(Int, String)]
files config = zip [0..] $ fmap show resultingSVGs
  where combinedShapes = fmap (staticSVGPart config <>) (dynamicSVGPart config)
        resultingSVGs = fmap (svg config) combinedShapes

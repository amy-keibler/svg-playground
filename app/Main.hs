{-# LANGUAGE OverloadedStrings #-}
module Main where

import Animation
import AnimationConfig
import SVG

import Graphics.Svg hiding (translate, toElement)
import Data.Text (pack)
import System.Environment (getArgs)

import Data.List (transpose)

filenameArg :: [String] -> String
filenameArg [] = undefined
filenameArg (filename':_) = filename'

main :: IO ()
main = getArgs >>= handleFileName
  where handleFileName (filepath:_) = loadConfig <$> readFile filepath >>= outputAnimiation
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
staticSVGPart = foldr1 (<>) . fmap toElement . staticShapes

dynamicSVGPart :: AnimationConfig -> [Element]
dynamicSVGPart config = animationsToElelemnts (numFrames config) (animatedShapes config)

files :: AnimationConfig -> [(Int, String)]
files config = zip [0..] $ fmap show resultingSVGs
  where combinedShapes = fmap (staticSVGPart config <>) (dynamicSVGPart config)
        resultingSVGs = fmap (svg config) combinedShapes

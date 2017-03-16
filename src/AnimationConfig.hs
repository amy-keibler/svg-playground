{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module AnimationConfig (AnimationConfig(..)
                       , SVGStyling(..)
                       , SVGShape(..)
                       , SVGAnimation(..)
                       , loadConfig)where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad (mzero)

import Animation(Rectangle, Line)

defaultFilename :: String
defaultFilename = "/tmp/animation"

defaultFrameSize :: Int
defaultFrameSize = 64

defaultNumFrames :: Int
defaultNumFrames = 10

data SVGStyling = SVGStyling {
  stroke :: String
  , strokeWidth :: String
  , fill :: String
  , fillOpacity :: String
  } deriving (Eq, Show, Generic)

instance FromJSON SVGStyling where
  parseJSON (Object v) = SVGStyling <$>
                         v .:? "stroke" .!= "black" <*>
                         v .:? "strokeWidth" .!= "1" <*>
                         v .:? "fill" .!= "white" <*>
                         v .:? "fillOpacity" .!= "1"
  parseJSON _ = mzero

data SVGShape = SVGShape {
  rect :: Rectangle
  , style :: SVGStyling
  } deriving (Eq, Show, Generic)

instance FromJSON SVGShape

data SVGAnimation = SVGAnimation {
  startLine :: Line
  , endLine :: Line
  , animatedStyle :: SVGStyling
  } deriving (Eq, Show, Generic)

instance FromJSON SVGAnimation

data AnimationConfig = AnimationConfig {
  filename :: String
  , frameWidth :: Int
  , frameHeight :: Int
  , numFrames :: Int
  , staticShapes :: [SVGShape]
  , animatedShapes :: [SVGAnimation]
  } deriving (Eq, Show, Generic)

instance FromJSON AnimationConfig where
  parseJSON (Object v) = AnimationConfig <$>
                         v .:? "filename" .!= defaultFilename <*>
                         v .:? "frameWidth" .!= defaultFrameSize <*>
                         v .:? "frameHeight" .!= defaultFrameSize <*>
                         v .:? "numFrames" .!= defaultNumFrames <*>
                         v .:? "staticShapes" .!= [] <*>
                         v .:? "animatedShapes" .!= []
  parseJSON _ = mzero

loadConfig :: String -> Either String AnimationConfig
loadConfig = eitherDecode . C.pack

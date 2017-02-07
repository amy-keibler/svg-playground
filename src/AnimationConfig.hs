{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module AnimationConfig (AnimationConfig(..)
                       , loadConfig)where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad (mzero)

defaultFilename :: String
defaultFilename = "/tmp/animation"

defaultFrameSize :: Int
defaultFrameSize = 64

defaultNumFrames :: Int
defaultNumFrames = 10

data AnimationConfig = AnimationConfig {
  filename :: String
  , frameWidth :: Int
  , frameHeight :: Int
  , numFrames :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON AnimationConfig where
  parseJSON (Object v) = AnimationConfig <$>
                         v .:? "filename" .!= defaultFilename <*>
                         v .:? "frameWidth" .!= defaultFrameSize <*>
                         v .:? "frameHeight" .!= defaultFrameSize <*>
                         v .:? "numFrames" .!= defaultNumFrames
  parseJSON _ = mzero

loadConfig :: String -> Either String AnimationConfig
loadConfig = eitherDecode . C.pack

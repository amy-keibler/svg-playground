module AnimationConfigSpec (main, spec) where

import Test.Hspec

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as C

import AnimationConfig
import Animation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "configuration" $ do
    it "should transform complete a configuration into a datatype" $
      loadConfig "{\"filename\": \"/tmp/test_file\", \"frameWidth\": 64, \"frameHeight\": 128, \"numFrames\": 10, \"staticShapes\": [{\"rect\": {\"topLeft\": {\"x\": 32, \"y\": 24}, \"bottomRight\": {\"x\": 24, \"y\": 32}}, \"style\": {\"stroke\": \"red\", \"strokeWidth\": \"2\", \"fill\": \"blue\", \"fillOpacity\": \"1\"}}]}" `shouldBe` Right AnimationConfig { filename="/tmp/test_file", frameWidth=64, frameHeight=128, numFrames=10, staticShapes=[SVGShape (Rectangle (Point 32 24) (Point 24 32)) (SVGStyling "red" "2" "blue" "1")]}
    it "should fill with defaults for an empty configuration" $
      loadConfig "{}" `shouldBe` Right AnimationConfig { filename="/tmp/animation", frameWidth=64, frameHeight=64, numFrames=10, staticShapes=[]}
  describe "styles" $
    it "should handle the default styles" $
      (decode . C.pack) "{}" `shouldBe` Just (SVGStyling "black" "1" "white" "1")

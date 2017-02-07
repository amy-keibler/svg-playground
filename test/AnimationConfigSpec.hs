module AnimationConfigSpec (main, spec) where

import Test.Hspec

import AnimationConfig

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "configuration" $ do
    it "should transform complete a configuration into a datatype" $
      loadConfig "{\"filename\": \"/tmp/test_file\", \"frameWidth\": 64, \"frameHeight\": 128, \"numFrames\": 10}" `shouldBe` Right AnimationConfig { filename="/tmp/test_file", frameWidth=64, frameHeight=128, numFrames=10}
    it "should fill with defaults for an empty configuration" $
      loadConfig "{}" `shouldBe` Right AnimationConfig { filename="/tmp/animation", frameWidth=64, frameHeight=64, numFrames=10}

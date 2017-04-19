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
      loadConfig "{\"filename\": \"/tmp/test_file\", \"frameWidth\": 64, \"frameHeight\": 128, \"numFrames\": 10, \"staticShapes\": [{\"line\": {\"start\": {\"x\": 32, \"y\": 24}, \"end\": {\"x\": 24, \"y\": 32}}, \"style\": {\"stroke\": \"red\", \"strokeWidth\": \"2\", \"fill\": \"blue\", \"fillOpacity\": \"1\"}}, {\"rect\": {\"topLeft\": {\"x\": 32, \"y\": 24}, \"bottomRight\": {\"x\": 24, \"y\": 32}}, \"style\": {\"stroke\": \"red\", \"strokeWidth\": \"2\", \"fill\": \"blue\", \"fillOpacity\": \"1\"}}, {\"circle\": {\"center\": {\"x\": 32, \"y\": 24}, \"radius\": 10}, \"style\": {\"stroke\": \"red\", \"strokeWidth\": \"2\", \"fill\": \"blue\", \"fillOpacity\": \"1\"}}, {\"ellipse\": {\"ellipseCenter\": {\"x\": 32, \"y\": 24}, \"rX\": 10, \"rY\": 12}, \"style\": {\"stroke\": \"red\", \"strokeWidth\": \"2\", \"fill\": \"blue\", \"fillOpacity\": \"1\"}}], \"animatedShapes\": [{\"startLine\": {\"start\": {\"x\": 0, \"y\": 0}, \"end\": {\"x\": 1, \"y\": 1}}, \"endLine\": {\"start\": {\"x\": 1, \"y\": 0}, \"end\": {\"x\": 0, \"y\": 1}}, \"animatedStyle\":{\"stroke\": \"white\", \"strokeWidth\": \"4\"}}, {\"startRect\": {\"topLeft\": {\"x\": 0, \"y\": 0}, \"bottomRight\": {\"x\": 1, \"y\": 1}}, \"endRect\": {\"topLeft\": {\"x\": 1, \"y\": 0}, \"bottomRight\": {\"x\": 0, \"y\": 1}}, \"animatedStyle\":{\"stroke\": \"white\", \"strokeWidth\": \"4\"}}, {\"startCircle\": {\"center\": {\"x\": 0, \"y\": 0}, \"radius\": 10}, \"endCircle\": {\"center\": {\"x\": 1, \"y\": 0}, \"radius\": 12}, \"animatedStyle\":{\"stroke\": \"white\", \"strokeWidth\": \"4\"}}, {\"startEllipse\": {\"ellipseCenter\": {\"x\": 0, \"y\": 0}, \"rX\": 10, \"rY\": 12}, \"endEllipse\": {\"ellipseCenter\": {\"x\": 1, \"y\": 0}, \"rX\": 12, \"rY\": 10}, \"animatedStyle\":{\"stroke\": \"white\", \"strokeWidth\": \"4\"}}]}" `shouldBe` Right AnimationConfig {
      filename="/tmp/test_file",
      frameWidth=64,
      frameHeight=128,
      numFrames=10,
      staticShapes=[SVGLine (Line (Point 32 24) (Point 24 32)) (SVGStyling "red" "2" "blue" "1" Nothing),
                    SVGRect (Rectangle (Point 32 24) (Point 24 32)) (SVGStyling "red" "2" "blue" "1" Nothing),
                    SVGCircle (Circle (Point 32 24) 10) (SVGStyling "red" "2" "blue" "1" Nothing),
                    SVGEllipse (Ellipse (Point 32 24) 10 12) (SVGStyling "red" "2" "blue" "1" Nothing)],
      animatedShapes=[SVGAnimationLine (Line (Point 0 0) (Point 1 1)) (Line (Point 1 0) (Point 0 1)) (SVGStyling "white" "4" "white" "1" Nothing),
                      SVGAnimationRect (Rectangle (Point 0 0) (Point 1 1)) (Rectangle (Point 1 0) (Point 0 1)) (SVGStyling "white" "4" "white" "1" Nothing),
                      SVGAnimationCircle (Circle (Point 0 0) 10) (Circle (Point 1 0) 12) (SVGStyling "white" "4" "white" "1" Nothing),
                      SVGAnimationEllipse (Ellipse (Point 0 0) 10 12) (Ellipse (Point 1 0) 12 10) (SVGStyling "white" "4" "white" "1" Nothing)]}
    it "should fill with defaults for an empty configuration" $
      loadConfig "{}" `shouldBe` Right AnimationConfig { filename="/tmp/animation", frameWidth=64, frameHeight=64, numFrames=10, staticShapes=[], animatedShapes=[]}
  describe "styles" $ do
    it "should handle the default styles" $
      (decode . C.pack) "{}" `shouldBe` Just (SVGStyling "black" "1" "white" "1" Nothing)
    it "should handle shape specific styles" $
      (decode . C.pack) "{\"shapeStyle\": {\"shape\": \"rect\", \"rx\": \"10\", \"ry\": \"10\"}}" `shouldBe` Just (SVGStyling "black" "1" "white" "1" (Just (RectStyle "10" "10")))
  describe "interpolateSVG" $ do
    it "should transform lines" $
      interpolateSVG 3 (SVGAnimationLine (Line (Point 0 0) (Point 4 2))
                         (Line (Point 2 4) (Point 0 0))
                         (SVGStyling "red" "2" "blue" "1" Nothing)) `shouldBe` [SVGLine (Line (Point 0 0) (Point 4 2))
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGLine (Line (Point 1 2) (Point 2 1))
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGLine (Line (Point 2 4) (Point 0 0))
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing)]
    it "should transform rectangles" $
      interpolateSVG 3 (SVGAnimationRect (Rectangle (Point 0 0) (Point 4 2))
                         (Rectangle (Point 2 4) (Point 0 0))
                         (SVGStyling "red" "2" "blue" "1" Nothing)) `shouldBe` [SVGRect (Rectangle (Point 0 0) (Point 4 2))
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGRect (Rectangle (Point 1 2) (Point 2 1))
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGRect (Rectangle (Point 2 4) (Point 0 0))
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing)]
    it "should transform circles" $
      interpolateSVG 3 (SVGAnimationCircle (Circle (Point 0 0) 10)
                         (Circle (Point 2 4) 20)
                         (SVGStyling "red" "2" "blue" "1" Nothing)) `shouldBe` [SVGCircle (Circle (Point 0 0) 10)
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGCircle (Circle (Point 1 2) 15)
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGCircle (Circle (Point 2 4) 20)
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing)]
    it "should transform ellipses" $
      interpolateSVG 3 (SVGAnimationEllipse (Ellipse (Point 0 0) 10 20)
                         (Ellipse (Point 2 4) 20 10)
                         (SVGStyling "red" "2" "blue" "1" Nothing)) `shouldBe` [SVGEllipse (Ellipse (Point 0 0) 10 20)
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGEllipse (Ellipse (Point 1 2) 15 15)
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing),
                                                                                SVGEllipse (Ellipse (Point 2 4) 20 10)
                                                                                 (SVGStyling "red" "2" "blue" "1" Nothing)]

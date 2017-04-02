module AnimationSpec (main, spec) where

import Test.Hspec

import Animation
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "iterate" $ do
    it "should iterate between zero and 2" $
      interpolate 3 0 2 `shouldBe` [0, 1, 2]
    it "should iterate between 10 and 50 for 9 frames" $
      interpolate 9 10 50 `shouldBe` [10, 15, 20, 25, 30, 35, 40, 45, 50]
    it "should iterate between 10 and 50 for 10 frames" $
      interpolate 10 10 50 `shouldBe` [10, 14, 19, 23, 28, 32, 37, 41, 46, 50]
  describe "animate typeclass" $ do
    it "should animate Points directly to end in two frames" $
      animate 2 (Point 0 1) (Point 1 0) `shouldBe` [Point 0 1, Point 1 0]
    it "should animate Points in-between the points once for three frames" $
      animate 3 (Point 0 2) (Point 2 0) `shouldBe` [Point 0 2, Point 1 1, Point 2 0]
    it "should animate the line over 2" $
      animate 3 (Line (Point 0 0) (Point 0 2))
        (Line (Point 2 0) (Point 2 2)) `shouldBe` [Line (Point 0 0) (Point 0 2),
                                                   Line (Point 1 0) (Point 1 2),
                                                   Line (Point 2 0) (Point 2 2)]
    it "should animate the rectangle over 2" $
      animate 3 (Rectangle (Point 0 0) (Point 2 2)) (Rectangle (Point 2 2) (Point 4 4)) `shouldBe` [Rectangle (Point 0 0) (Point 2 2),
                                                                                                    Rectangle (Point 1 1) (Point 3 3),
                                                                                                    Rectangle (Point 2 2) (Point 4 4)]
    it "should animate the circle over 2" $
      animate 3 (Circle (Point 0 0) 2) (Circle (Point 2 2) 4) `shouldBe` [Circle (Point 0 0) 2,
                                                                          Circle (Point 1 1) 3,
                                                                          Circle (Point 2 2) 4]
    it "should animate the ellipse over 2" $
      animate 3 (Ellipse (Point 0 0) 2 4) (Ellipse (Point 2 2) 4 2) `shouldBe` [Ellipse (Point 0 0) 2 4,
                                                                                Ellipse (Point 1 1) 3 3,
                                                                                Ellipse (Point 2 2) 4 2]
  describe "translate typeclass" $ do
    it "should translate points" $
      translate (Point 1 1) (Point 0 1) `shouldBe` Point 1 2
    it "should translate lines" $
      translate (Point 1 1) (Line (Point 0 1) (Point 1 0)) `shouldBe` Line (Point 1 2) (Point 2 1)
    it "should translate rectangles" $
      translate (Point 1 1) (Rectangle (Point 0 1) (Point 1 0)) `shouldBe` Rectangle (Point 1 2) (Point 2 1)
    it "should translate circles" $
      translate (Point 1 1) (Circle (Point 0 1) 5) `shouldBe` Circle (Point 1 2) 5
    it "should translate ellipses" $
      translate (Point 1 1) (Ellipse (Point 0 1) 5 6) `shouldBe` Ellipse (Point 1 2) 5 6
  describe "json" $ do
    it "should decode points" $
      (decode . C.pack) "{\"x\": 32, \"y\": 24}" `shouldBe` Just Point {x=32, y=24}
    it "should decode lines" $
      (decode . C.pack) "{\"start\": {\"x\": 32, \"y\": 24}, \"end\": {\"x\": 24, \"y\": 32}}" `shouldBe` Just (Line Point {x=32, y=24} Point {x=24, y=32})
    it "should decode rectangles" $
      (decode . C.pack) "{\"topLeft\": {\"x\": 32, \"y\": 24}, \"bottomRight\": {\"x\": 24, \"y\": 32}}" `shouldBe` Just (Rectangle (Point 32 24) (Point 24 32))
    it "should decode circles" $
      (decode . C.pack) "{\"center\": {\"x\": 1, \"y\": 2}, \"radius\": 3}" `shouldBe` Just (Circle (Point 1 2) 3)
    it "should decode ellipses" $
      (decode . C.pack) "{\"ellipseCenter\": {\"x\": 1, \"y\": 2}, \"rX\": 3, \"rY\": 4}" `shouldBe` Just (Ellipse (Point 1 2) 3 4)

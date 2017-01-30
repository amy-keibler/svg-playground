module AnimationSpec (main, spec) where

import Test.Hspec

import Animation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "linearAnimate" $ do
    it "should animate directly to end in two frames" $
      linearAnimate 2 (Point 0 1) (Point 1 0) `shouldBe` [Point 0 1, Point 1 0]
    it "should animate in-between the points once for three frames" $
      linearAnimate 3 (Point 0 2) (Point 2 0) `shouldBe` [Point 0 2, Point 1 1, Point 2 0]
  describe "iterate" $ do
    it "should iterate between zero and 2" $
      interpolate 3 0 2 `shouldBe` [0, 1, 2]
    it "should iterate between 10 and 50" $
      interpolate 9 10 50 `shouldBe` [10, 15, 20, 25, 30, 35, 40, 45, 50]
  describe "linearAnimateLine" $
    it "should interpolate the line over 2" $
      linearAnimateLine 3 (Line (Point 0 0) (Point 0 2))
        (Line (Point 2 0) (Point 2 2)) `shouldBe` [Line (Point 0 0) (Point 0 2),
                                                   Line (Point 1 0) (Point 1 2),
                                                   Line (Point 2 0) (Point 2 2)]
  describe "translate typeclass" $ do
    it "should translate points" $
      translate (Point 1 1) (Point 0 1) `shouldBe` Point 1 2
    it "should translate lines" $
      translate (Point 1 1) (Line (Point 0 1) (Point 1 0)) `shouldBe` Line (Point 1 2) (Point 2 1)

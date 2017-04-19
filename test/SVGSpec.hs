module SVGSpec (spec) where

import Test.Hspec

import AnimationConfig
import Animation

import SVG

spec :: Spec
spec = do
  describe "animations to elements" $
    it "should animate a list of svg elements into frames" $
      fmap show (animationsToElelemnts 3 [SVGAnimationLine (Line (Point 0 0) (Point 0 4))
                                           (Line (Point 2 2) (Point 4 0))
                                           (SVGStyling "red" "2" "blue" "4" Nothing),
                                          SVGAnimationRect (Rectangle (Point 0 0) (Point 0 4))
                                           (Rectangle (Point 2 2) (Point 4 0))
                                           (SVGStyling "red" "2" "blue" "4" Nothing)]) `shouldBe` ["<line x2=\"0\" y1=\"0\" stroke=\"red\" stroke-width=\"2\" y2=\"4\" x1=\"0\"/><rect height=\"4\" fill-opacity=\"4\" width=\"0\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" x=\"0\" y=\"0\"/>",
                                                                                           "<line x2=\"2\" y1=\"1\" stroke=\"red\" stroke-width=\"2\" y2=\"2\" x1=\"1\"/><rect height=\"1\" fill-opacity=\"4\" width=\"1\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" x=\"1\" y=\"1\"/>",
                                                                                           "<line x2=\"4\" y1=\"2\" stroke=\"red\" stroke-width=\"2\" y2=\"0\" x1=\"2\"/><rect height=\"2\" fill-opacity=\"4\" width=\"2\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" x=\"2\" y=\"0\"/>"]
  describe "svg to element" $ do
    it "should transform an SVGLine into an SVG element" $
      show (toElement (SVGLine (Line (Point 0 0) (Point 1 1)) (SVGStyling "red" "2" "blue" "4" Nothing))) `shouldBe` "<line x2=\"1\" y1=\"0\" stroke=\"red\" stroke-width=\"2\" y2=\"1\" x1=\"0\"/>"
    it "should transform an SVGRect into an SVG element" $
      show (toElement (SVGRect (Rectangle (Point 0 0) (Point 1 1)) (SVGStyling "red" "2" "blue" "4" (Just (RectStyle "10" "20"))))) `shouldBe` "<rect height=\"1\" rx=\"10\" fill-opacity=\"4\" ry=\"20\" width=\"1\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" x=\"0\" y=\"0\"/>"
    it "should transform an SVGRect into an SVG element with a backwards rectangle" $
      show (toElement (SVGRect (Rectangle (Point 1 1) (Point 0 0)) (SVGStyling "red" "2" "blue" "4" Nothing))) `shouldBe` "<rect height=\"1\" fill-opacity=\"4\" width=\"1\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" x=\"0\" y=\"0\"/>"
    it "should transform an SVGCircle into an SVG element" $
      show (toElement (SVGCircle (Circle (Point 0 0) 10) (SVGStyling "red" "2" "blue" "4" Nothing))) `shouldBe` "<circle fill-opacity=\"4\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" cy=\"0\" r=\"10\" cx=\"0\"/>"
    it "should transform an SVGEllipse into an SVG element" $
      show (toElement (SVGEllipse (Ellipse (Point 0 0) 10 20) (SVGStyling "red" "2" "blue" "4" Nothing))) `shouldBe` "<ellipse rx=\"10\" fill-opacity=\"4\" ry=\"20\" stroke=\"red\" stroke-width=\"2\" fill=\"blue\" cy=\"0\" cx=\"0\"/>"

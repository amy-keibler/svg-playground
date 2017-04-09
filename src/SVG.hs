module SVG (animationsToElelemnts
           , toElement) where

import Animation
import AnimationConfig

import Graphics.Svg hiding (translate, toElement)
import Data.Text (pack)
import Data.List (transpose)

animationsToElelemnts :: Int -> [SVGAnimation] -> [Element]
animationsToElelemnts num animations = fmap frameToElement frames
  where shapes = fmap (interpolateSVG num) animations
        frames = transpose shapes
        frameToElement = foldr1 (<>) . fmap toElement

toElement :: SVGShape -> Element
toElement (SVGLine (Line (Point xS yS) (Point xE yE))
           (SVGStyling color width _ _)) = line_ [X1_ <<- pack (show xS), Y1_ <<- pack (show yS),
                                                  X2_ <<- pack (show xE), Y2_ <<- pack (show yE),
                                                  Stroke_ <<- pack color, Stroke_width_ <<- pack width]
toElement (SVGRect (Rectangle (Point xS yS) (Point xE yE)) styling) = rect_ [X_ <<- pack (show rectX),
                                                                             Y_ <<- pack (show rectY),
                                                                             Width_ <<- pack (show width),
                                                                             Height_ <<- pack (show height),
                                                                             Stroke_ <<- pack (stroke styling),
                                                                             Stroke_width_ <<- pack (strokeWidth styling),
                                                                             Fill_ <<- pack (fill styling),
                                                                             Fill_opacity_ <<- pack (fillOpacity styling)]
  where rectX = min xS xE
        rectY = min yS yE
        width = abs $ xS - xE
        height = abs $ yS - yE
toElement (SVGCircle circle styling) = circle_ [Cx_ <<- pack ((show . x . center) circle),
                                                Cy_ <<- pack ((show . y . center) circle),
                                                R_ <<- pack ((show . radius) circle),
                                                Stroke_ <<- pack (stroke styling),
                                                Stroke_width_ <<- pack (strokeWidth styling),
                                                Fill_ <<- pack (fill styling),
                                                Fill_opacity_ <<- pack (fillOpacity styling)]
toElement (SVGEllipse ellipse styling) = ellipse_ [Cx_ <<- pack ((show . x . ellipseCenter) ellipse),
                                                  Cy_ <<- pack ((show . y . ellipseCenter) ellipse),
                                                  Rx_ <<- pack ((show . rX) ellipse),
                                                  Ry_ <<- pack ((show . rY) ellipse),
                                                  Stroke_ <<- pack (stroke styling),
                                                  Stroke_width_ <<- pack (strokeWidth styling),
                                                  Fill_ <<- pack (fill styling),
                                                  Fill_opacity_ <<- pack (fillOpacity styling)]

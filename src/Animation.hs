{-# LANGUAGE DeriveGeneric #-}
module Animation (Point(..)
                 , Line(..)
                 , Rectangle(..)
                 , Circle(..)
                 , Ellipse(..)
                 , Translatable(..)
                 , Animatable(..)
                 , interpolate) where

import GHC.Generics
import Data.Aeson

-- typeclasses
class Translatable a where
  translate :: Point -> a -> a

class Animatable a where
  animate :: Int -> a -> a -> [a]

-- interpolation
interpolate :: Int -> Int -> Int -> [Int]
interpolate num is ie = take num $ round <$> (iterate (+ (diff :: Double)) . fromIntegral) is
  where diff = fromIntegral (ie - is) / fromIntegral (num - 1)

-- Point
data Point = Point {
  x :: Int
  , y :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Point

instance Translatable Point where
  translate (Point xt yt) (Point xp yp) = Point (xt + xp) (yt + yp)

instance Animatable Point where
  animate num (Point xs ys) (Point xe ye) = zipWith Point xPoints yPoints
    where xPoints = interpolate num xs xe
          yPoints = interpolate num ys ye

-- Line
data Line = Line {
  start :: Point
  , end :: Point
  } deriving (Eq, Show, Generic)

instance FromJSON Line

instance Translatable Line where
  translate pt (Line pStart pEnd) = Line (translate pt pStart) (translate pt pEnd)

instance Animatable Line where
  animate num (Line ps0 ps1) (Line pe0 pe1) = zipWith Line p0Points p1Points
    where p0Points = animate num ps0 pe0
          p1Points = animate num ps1 pe1

-- Rectangle
data Rectangle = Rectangle {
  topLeft :: Point
  , bottomRight :: Point
  } deriving (Eq, Show, Generic)

instance FromJSON Rectangle

instance Translatable Rectangle where
  translate pt (Rectangle pTL pBR) = Rectangle (translate pt pTL) (translate pt pBR)

instance Animatable Rectangle where
  animate num (Rectangle ps0 ps1) (Rectangle pe0 pe1) = zipWith Rectangle p0Points p1Points
    where p0Points = animate num ps0 pe0
          p1Points = animate num ps1 pe1

-- Circle
data Circle = Circle {
  center :: Point
  , radius :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Circle

instance Translatable Circle where
  translate pt (Circle c r) = Circle (translate pt c) r

instance Animatable Circle where
  animate num (Circle ps rs) (Circle pe re) = zipWith Circle points radiuses
    where points = animate num ps pe
          radiuses = interpolate num rs re

-- Ellipse
data Ellipse = Ellipse {
  ellipseCenter :: Point
  , rX :: Int
  , rY :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Ellipse

instance Translatable Ellipse where
  translate pt (Ellipse c radX radY) = Ellipse (translate pt c) radX radY

instance Animatable Ellipse where
  animate num (Ellipse ps rs0 rs1) (Ellipse pe re0 re1) = zipWith3 Ellipse points r0s r1s
    where points = animate num ps pe
          r0s = interpolate num rs0 re0
          r1s = interpolate num rs1 re1

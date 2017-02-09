{-# LANGUAGE DeriveGeneric #-}
module Animation (Point(..)
                 , Line(..)
                 , Rectangle(..)
                 , Translatable(..)
                 , linearAnimate
                 , linearAnimateLine
                 , interpolate) where

import GHC.Generics
import Data.Aeson

class Translatable a where
  translate :: Point -> a -> a

data Point = Point {
  x :: Int
  , y :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Point

instance Translatable Point where
  translate (Point xt yt) (Point x y) = Point (xt + x) (yt + y)

data Line = Line {
  start :: Point
  , end :: Point
  } deriving (Eq, Show, Generic)

instance FromJSON Line

instance Translatable Line where
  translate pt (Line pStart pEnd) = Line (translate pt pStart) (translate pt pEnd)

data Rectangle = Rectangle {
  topLeft :: Point
  , bottomRight :: Point
  } deriving (Eq, Show, Generic)

instance FromJSON Rectangle

instance Translatable Rectangle where
  translate pt (Rectangle topLeft bottomRight) = Rectangle (translate pt topLeft) (translate pt bottomRight)

linearAnimateLine :: Int -> Line -> Line -> [Line]
linearAnimateLine num (Line start0 end0) (Line start1 end1) = zipWith Line startPoints endPoints
  where startPoints = linearAnimate num start0 start1
        endPoints = linearAnimate num end0 end1

linearAnimate :: Int -> Point -> Point -> [Point]
linearAnimate num (Point x0 y0) (Point x1 y1) = zipWith Point xPoints yPoints
  where xPoints = interpolate num x0 x1
        yPoints = interpolate num y0 y1

interpolate :: Int -> Int -> Int -> [Int]
interpolate num start stop = take num $ round <$> (iterate (+ diff) . fromIntegral) start
  where diff = fromIntegral (stop - start) / fromIntegral (num - 1)

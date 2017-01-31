module Animation (Point(..), Line(..), Translatable(..), linearAnimate, linearAnimateLine, interpolate) where

class Translatable a where
  translate :: Point -> a -> a

data Point = Point Int Int deriving (Eq, Show)

instance Translatable Point where
  translate (Point xt yt) (Point x y) = Point (xt + x) (yt + y)

data Line = Line Point Point deriving (Eq, Show)

instance Translatable Line where
  translate pt (Line pStart pEnd) = Line (translate pt pStart) (translate pt pEnd)

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

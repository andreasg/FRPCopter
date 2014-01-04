{-# LANGUAGE NoMonomorphismRestriction #-}
module Level where

import Params
import Control.Wire
import System.Random
import Prelude hiding ((.), id, floor, ceiling)
import Control.Wire.Unsafe.Event
import Data.Fixed (mod')
import Control.Monad.Reader
import Linear (V2(..))


class CanContain a where
  contains :: V2 Double -> a -> Bool
  
data Rect = Rect (V2 Double) (V2 Double)

instance CanContain Rect where
  contains (V2 x y) (Rect (V2 x' y') (V2 w h)) = x' <= x && x <= (x'+w) && y' <= y && y <= (y'+h)

rects
  :: (HasTime t s, Fractional t, RandomGen g, Monoid e) =>
     (Double, Double) -> (Double, Double) -> g -> Wire s e (Reader GameParams) Double (Event Rect)
rects interv valueRange gen =
  let (t' :: Double, g') = randomR interv gen
      t = realToFrac t'
  in mkGenN $ \_ ->
       do gp <- ask
          return (Right NoEvent, go (scrollSpeed gp) g' t t)
  where
  go s g t 0 = go s g t t
  go s g t t' =
          mkSF $ \ds xcord ->
           let dt = t' -  dtime ds
           in if dt <= 0
              then let (y, g') = randomR valueRange g
                       (t'' :: Double, g'') = randomR interv g'
                       timeToNext = realToFrac t''
                   in (Event (Rect (V2 xcord (round' y)) (V2 (timeToNext * s + 1) magicHeight)), go s g'' timeToNext (dt `mod'` timeToNext))
              else (NoEvent, go s g t dt)
  round' = fromInteger . round
  

magicHeight = 30

nextCoord
  :: (HasTime t s, Fractional t, RandomGen g, Monoid e, Monad m) =>
     (Double, Double) -> (Double, Double) -> g -> Wire s e m Double (Event Point)
nextCoord interv valueRange gen =
  let (x :: Double, g') = randomR valueRange gen
      (t :: Double, g'') = randomR interv g'
      y = realToFrac t
  in mkSFN $ \xcord -> (Event (V2 xcord (round' x)), go g'' y y)
  where
  go g t 0 = go g t t
  go g t t' =
          mkSF $ \ds xcord ->
           let dt = t' -  dtime ds
           in if dt <= 0
              then let (x, g') = randomR valueRange g
                       (t'' :: Double, g'') = randomR interv g'
                       y = realToFrac t''
                   in (Event (V2 xcord (round' x)), go g'' y (dt `mod'` y))
              else (NoEvent, go g t dt)
  round' = fromInteger . round


type Point = V2 Double

data Ceiling = Ceiling  {
    unCeiling :: ![Point]
  } deriving (Show)


data Floor = Floor {
    unFloor :: ![Point]
  } deriving (Show)

outOfRangeThreshhold :: Num a => a
outOfRangeThreshhold = 1000


rectToCeiling (Rect (V2 x y) (V2 w h)) = Rect (V2 x 0) (V2 w y)
rectToFloor (Rect (V2 x y) (V2 w h)) = Rect (V2 x y) (V2 w (screenH - y))

level :: (Fractional t, Monoid e, HasTime t s, RandomGen g) => g -> Wire s e (Reader GameParams) a ([Rect], [Rect])
level gen =
  let (g0, g1) = split gen
  in mkGenN $ \_ -> do
    gs <- ask
    return (Left mempty, arr (map rectToCeiling) . accum . bounds gs ceilingRange g0 &&& arr (map rectToFloor) . accum . bounds gs floorRange g1)
  where
  accum = hold . accumE mrg []
  mrg xs x@(Rect (V2 x0 _) _ ) = x : filter (\(Rect (V2 x1 _) _ ) -> x1 >= x0 - screenW - outOfRangeThreshhold) xs
  bounds gp range g =
        rects (segmentLength gp) (range gp) g
               . arr ((+) screenW
                      . fromInteger
                      . round
                      . (realToFrac ::  Real a => a -> Double))
               . (time * pure (scrollSpeed gp))


level' :: (Fractional t, Monoid e, HasTime t s, RandomGen g) => g -> Wire s e (Reader GameParams) a (Ceiling, Floor)
level' gen =
  let (g0, g1) = split gen
  in mkGenN $ \_ -> do
    gs <- ask
    return (Left mempty, arr Ceiling . accum . bounds gs ceilingRange g0 &&& arr Floor . accum .  bounds gs floorRange g1)
  where
  accum = hold . accumE mrg []
  mrg xs x@(V2 x0 _) = x : filter (\(V2 x1 _) -> x1 >= x0 - screenW - outOfRangeThreshhold) xs
  bounds gp range g =
        nextCoord (segmentLength gp) (range gp) g
               . arr ((+) screenW
                      . fromInteger
                      . round
                      . (realToFrac ::  Real a => a -> Double))
               . (time * pure (scrollSpeed gp))

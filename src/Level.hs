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
  
data Rect = Rect (V2 Double) (V2 Double) deriving (Show)

instance CanContain Rect where
  contains (V2 x y) (Rect (V2 x' y') (V2 w h)) = x' <= x && x <= (x'+w) && y' <= y && y <= (y'+h)



events :: (HasTime t s, Fractional t, RandomGen g) =>
     (Double, Double) -> g -> Wire s () m Double (Event (Double, Double, g))
events interv gen = 
  let (t, g') = toTime $ randomR interv gen
  in mkSFN $ \_ -> (NoEvent, go g' t t)
  where go g tt 0 = go g tt tt
        go g tt t = do
          mkSF $ \ds xcord ->
            let dt = t - dtime ds
            in if dt >  0
               then (NoEvent, go g tt dt)
               else let (nextWait, g') =  toTime $ randomR interv g
                        (g0, g1) = split g'
                    in (Event (fromInteger . round $ xcord, nextWait, g0), go g1 ( nextWait) (mod' dt ( nextWait)))
        toTime (x,y) = ((fromInteger . round $ 100.0*x) / 100.0, y)




--rects' g = hold . toRect (0,10) (0,10) (0, 10) (0,10) . events (1,3) g

ceiling'
  :: (HasTime t s, RandomGen g, Fractional t) =>
     g -> Wire s () (Reader GameParams) a [Rect]
ceiling' g = mkGenN $ \_ -> do
  gp <- ask
  return (Left mempty, hold . accumE (flip (:)) [] . ceilingTile (scrollSpeed gp) (ceilingRange gp) . events (segmentLength gp) g . arr realToFrac .  (time* pure (scrollSpeed gp)))


bounds cf ff g =
  let (g0, g1) = split g
  in mkGenN $ \_ -> do
  gp <- ask
  return (Left mempty,
          hold . accumE (flip (:)) [] . ceilingTile (scrollSpeed gp) (cf gp) . events (segmentLength gp) g0 &&&
          hold . accumE (flip (:)) [] . ceilingTile (scrollSpeed gp) (ff gp) . events (segmentLength gp) g1 . arr realToFrac .  (time* pure (scrollSpeed gp)))
  

ceilingTile spd ycord =
  arr . fmap $ \(x, dt, g) ->
     Rect (V2 x 0) (V2 (n $ dt*spd) (n (fst $ randomR ycord g :: Double)))

floorTile spd ycord =
  arr . fmap $ \(x, dt, g) ->
     let (y :: Double, _) = randomR ycord g in Rect (V2 (n x) (n y)) (V2 (n $ dt*spd) (n $ screenH - y))


toRect xcord ycord width height =
  arr . fmap $ \(x, dt, g) ->
    let (x, g0) = randomR xcord g
        (y, g1) = randomR ycord g0
        (w, g2) = randomR width g1
        (h, _) = randomR height g2
    in  Rect (V2 (n x) (n y)) (V2 (n w) (n h))


n x = (fromInteger . round $ x*100) / 100.0


rects
  :: (HasTime t s, Fractional t, RandomGen g, Monoid e) => Double ->
     (Double, Double) -> (Double, Double) -> (Double, Double) -> g -> Wire s e (Reader GameParams) Double (Event Rect)
rects widthCoef interv yrange hrange gen =
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
              then let (y, g') = randomR yrange g
                       (h, g'') = randomR hrange g'
                       (t'' :: Double, g''') = randomR interv g''
                       timeToNext = realToFrac t''
                       w = (timeToNext * s * widthCoef) + 1                       
                   in (Event (Rect (V2 xcord (round' y)) (V2 w h)), go s g''' timeToNext (dt `mod'` timeToNext))
              else (NoEvent, go s g t dt)
  round' = fromInteger . round
  


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



level :: (Fractional t, Monoid e, HasTime t s, RandomGen g) => g -> Wire s e (Reader GameParams) a ([Rect], ([Rect], [Rect]))
level gen =
  let (g0, g1) = split gen
  in mkGenN $ \_ -> do
    gs <- ask
    return (Left mempty,
            (accum . bounds 0.1 gs ((\(x0, x1) -> (x0*4, x1*4)) .segmentLength) obsticleRange obsticleHeights  g0) &&&
            (arr (map rectToCeiling) . accum . bounds 1 gs segmentLength ceilingRange (const (0,0)) g0 &&&
             arr (map rectToFloor) . accum . bounds 1 gs segmentLength floorRange (const (0,0)) g1))
  where
  outOfRangeThreshhold = 1000
  rectToCeiling (Rect (V2 x y) (V2 w _)) = Rect (V2 x 0) (V2 w y)
  rectToFloor (Rect (V2 x y) (V2 w _)) = Rect (V2 x y) (V2 w (screenH - y))    
  accum = hold . accumE mrg []
  mrg xs x@(Rect (V2 x0 _) _ ) = x : filter (\(Rect (V2 x1 _) _ ) -> x1 >= x0 - screenW - outOfRangeThreshhold) xs
  bounds c gp segment range hrange g =
        rects c (segment gp ) (range gp) (hrange gp) g
               . arr ((+) screenW
                      . fromInteger
                      . round
                      . (realToFrac ::  Real a => a -> Double))
               . (time * pure (scrollSpeed gp))

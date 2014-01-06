{-# LANGUAGE FlexibleContexts #-}
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


events :: (HasTime t s, Fractional t, RandomGen g, Monoid e) =>
     (Double, Double) -> g -> Wire s e m Double (Event (Double, Double, g))
events interv gen =
  let (t, g') = toTime $ randomR interv gen
  in mkSFN $ \_ -> (Event (0, 0, snd (split g')), go g' t t)
  where go g tt 0 = go g tt tt
        go g tt t = do
          mkSF $ \ds xcord ->
            let dt = t - dtime ds
            in if dt >  0
               then (NoEvent, go g tt dt)
               else let (nextWait, g') =  toTime $ randomR interv g
                        (g0, g1) = split g'
                    in (Event (fromInteger . round $ xcord, nextWait, g0), go g1 ( nextWait) (mod' dt ( nextWait)))
        toTime (x,y) = (n x, y)


level :: (Fractional t, Monoid e, HasTime t s, RandomGen g, MonadReader GameParams m) => g -> Wire s e m a ([Rect], ([Rect], [Rect]))
level g =
  let (g0, g') = split g
      (g1, g2) = split g'
  in scroll >>> obsticles g0 &&& ceiling g1 &&& floor g2


floor :: (RandomGen g, HasTime t s, Fractional t, Monoid e, MonadReader GameParams m) => g -> Wire s e m Double [Rect]
floor g' = mkGenN $ \_ -> do
  gp <- ask
  return (Right [], construct . floorTile (scrollSpeed gp) (floorRange gp) . events (1,1.4) g')
  where floorTile spd ycord =
          arr . fmap $ \(x, dt, g) ->
          let (y :: Double, _) = randomR ycord g in Rect (V2 (n x) (n y)) (V2 (n $ dt*spd) (n $ screenH - y))


ceiling :: (RandomGen g, HasTime t s, Fractional t, Monoid e, MonadReader GameParams m) => g -> Wire s e m Double [Rect]
ceiling g' = mkGenN $ \_ -> do
  gp <- ask
  return (Right [], construct . ceilingTile (scrollSpeed gp) (ceilingRange gp) . events (1,1.4) g')
  where ceilingTile spd ycord =
          arr . fmap $ \(x, dt, g) ->
          Rect (V2 x 0) (V2 (n $ dt*spd) (n (fst $ randomR ycord g :: Double)))


obsticles :: (RandomGen g, HasTime t s, Fractional t, Monoid e, MonadReader GameParams m) => g -> Wire s e m Double [Rect]
obsticles g = mkGenN $ \_ -> do
  gp <- ask
  return (Right [], construct . toRect (0,0) (obsticleRange gp) (obsticleHeights gp) (10,100) . events (1,1.4) g)


scroll :: (Fractional b, HasTime b1 s, Monoid e, MonadReader GameParams m) => Wire s e m a b
scroll =
  mkGenN $ \_ -> do
    gp <- ask
    return $ (Left mempty, arr realToFrac .  (time * pure (scrollSpeed gp)) + screenW)


construct :: (Monad m, Monoid e) => Wire s e m (Event Rect) [Rect]
construct = hold . accumE append []
 where append xs x@(Rect (V2 x0 _) _ ) = x : filter (\(Rect (V2 x1 _) _ ) -> x1 >= x0 - screenW - outOfRangeThreshhold) xs
       outOfRangeThreshhold = 1000


toRect :: (Functor f, Arrow a, RandomGen g) => (Double, Double)
       -> (Double, Double) -> (Double, Double) -> (Double, Double) -> a (f (Double, t, g)) (f Rect)
toRect xcord ycord width height =
  arr . fmap $ \(dx, _, g) ->
    let (x, g0) = randomR xcord g
        (y, g1) = randomR ycord g0
        (w, g2) = randomR width g1
        (h, _) = randomR height g2
    in  Rect (V2 (n (x+dx)) (n y)) (V2 (n w) (n h))


n :: (Fractional b, RealFrac a) => a -> b
n x = (fromInteger . round $ x*10) / 10.0

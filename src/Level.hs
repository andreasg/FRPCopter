module Level where

import Params
import Control.Wire
import System.Random
import Prelude hiding ((.), id, floor, ceiling)
import Linear
import Control.Monad.Fix
import FRP.Netwire
import Control.Wire.Unsafe.Event
import Data.Fixed (mod')

import Data.Time.Clock

data Level = Level {
  floor   :: [V2 Double]
 ,ceiling :: [V2 Double]
 } deriving (Show)


--level :: (HasTime t s, Monad m, Monoid e, RandomGen g) => g => Wire s e m a a
--level g = proc _ -> do
--  x <- (time * scrollSpeed) -< ()
--  (c,f) <- levelBorders g -< ()
--  _ <- after 1 -< ()
--  returnA -< undefined


scrollSpeed :: (HasTime t s, Monad m) => Wire s e m a t
scrollSpeed = pure 2

-- delay-range between generating new segments.
segmentInterval :: (Integer, Integer)
segmentInterval = (1, 3)

floorInterval :: (Double, Double)
floorInterval = (fromIntegral screenH / 2.0,  fromIntegral screenH)

ceilingInterval :: (Double, Double)
ceilingInterval = (0.0, fromIntegral screenH / 2.0)

--levelBorders :: (HasTime t s, Monad m, RandomGen g, Monoid e) => g -> Wire s e m a (Double, Double)
--levelBorders g = 
--  let (g0, g1) = split g
--  in nextFloorHeight ceilingInterval g0 1 &&& nextFloorHeight floorInterval g1 1

nextFloorHeight :: (RandomGen g, MonadFix m, Monad m, Monoid e, HasTime t s) => (Double, Double) -> g -> t -> Wire s e m a Double
nextFloorHeight interv g t' =
--  let (x' :: Double, g0) = randomR interv g
--      x :: Double = fromIntegral $ (round x' :: Integer)
--      (nextWait :: Integer, g1) = randomR segmentInterval g0
  proc _ -> do
    rec
     gen <- arr (snd . next) -< newGen
     x <- returnA . after 1 -< oldX
     
--     (nextWait :: Integer, g1) <- arr (randomR segmentInterval) -< gen
     (nextX :: Double, _) <- arr (randomR interv) -<  gen


     oldX <- returnA -< nextX
     newGen <- delay g  -< gen
    _ <- after 1 -< ()
    returnA -< nextX
 

{-
  let (x' :: Double, g0) = randomR interv g
      x :: Double = fromIntegral $ (round x' :: Integer)
      (nextWait :: Integer, g1) = randomR segmentInterval g0
  in for 2 . (mkPureN $ \_ -> (Right x,  nextFloorHeight interv g1 (fromInteger nextWait))) . after 1
-}



nextFloor'
  :: (HasTime t s, RandomGen g, Monoid e, Monad m) =>
     (Integer, Integer) -> (Double, Double) -> g -> Wire s e m Double (Event (Double, Double))
nextFloor' interv valueRange gen =
  let (x :: Double, g') = randomR valueRange gen
      (t :: Integer, g'') = randomR interv g'
      y = fromInteger t
  in mkSFN $ \xcord -> (Event (xcord, round' x), go g'' y y)

  where
  go g t 0 = go g t t
  go g t t' =
          mkSF $ \ds xcord ->
           let dt = t' -  dtime ds
           in if dt <= 0
              then let (x, g') = randomR valueRange g
                       (t'' :: Integer, g'') = randomR interv g'
                       y = fromInteger t''
                   in (Event (xcord, round' x), go g'' y (dt `mod'` y))
              else (NoEvent, go g t dt)
  round' = fromInteger . round


          

newtype Ceiling = Ceiling  {
    unCeiling :: [(Double, Double)]
  } deriving (Show)

             
newtype Floor = Floor {
    unFloor :: [(Double, Double)]
  } deriving (Show)



level :: (Monad m, Monoid e, HasTime t s, RandomGen g) => g -> Wire s e m a (Ceiling, Floor)
level g =
  let (g0, g1) = split g
  in (arr Ceiling . accum . bounds ceilingInterval g0
       &&&
      arr Floor . accum . bounds floorInterval g1)

  where
  accum = hold . accumE (flip (:)) []
  bounds c g = nextFloor' segmentInterval c g . arr ((+) (fromIntegral screenW) . fromInteger . round . realToFrac) . (time * 2)


nextFloorE
  :: (Monad m, HasTime t s, RandomGen g) =>
     g -> t -> Wire s () m a (Event Double)
nextFloorE g t =
  let (t', g') = randomR (1,5) g
  in once . noiseR t (0, 640) g --> nextFloorE g' (fromInteger t')


   --(at t . pure v  --> nextFloorE g'' (fromInteger t'))

     --(now . pure v --> nextFloorE g'' (fromInteger t')) . after t 




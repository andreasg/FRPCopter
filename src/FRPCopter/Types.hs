module FRPCopter.Types where

import Linear hiding (point)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Random
import Control.Monad.Identity
import Data.Monoid (Monoid(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

--------------------------------------------------------------------------------
data GameParams = GameParams {
    scrollSpeed :: Num a => a
  , segmentLength :: (Double, Double)
  , ceilingRange :: (Double, Double)
  , floorRange :: (Double, Double)
  , outOfRangeLimit :: Num a => a
  , obsticleRange :: (Double, Double)
  , obsticleHeights :: (Double, Double)
  , scrW :: Num a => a
  , scrH :: Num a => a
  , bpp :: Int
  , accel :: Double
  , gravityForce :: Double
  , initGravity :: Double
  , bgWidth :: Double
  , playerSize :: V2 Double
 }

data Assets = Assets { font :: !TTF.Font
                     , background :: !SDL.Surface
                     , heli :: ![SDL.Surface]
                     , cloud :: !SDL.Surface
                     , borderColor :: !SDL.Pixel
                     , wallColor :: !SDL.Pixel
                     , redColor :: !SDL.Pixel}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
newtype GameState g a = GameState {
  runGameState :: ReaderT GameParams (RandT g Identity) a
  } deriving (Monad, MonadReader GameParams, MonadRandom)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data Particle = SmokePuff Point

data Game = Running { cameraPos :: !Double
                    , level :: !Level
                    , particles :: ![Particle]
                    , playerPos :: !Point
                    , bgSlice :: !(Rect, Maybe Rect)
                    , playerFrame :: !Int }
          | MainMenu Double
          | Ending

emptyGame :: Game
emptyGame = Running { cameraPos = 0
                    , level  = Level [] [] []
                    , particles = []
                    , playerPos = mkPoint 0 0
                    , bgSlice = (mkRect 0 0 0 0, Nothing)
                    , playerFrame = 0}


data GameOver = GameOver !Integer deriving Show

instance Monoid GameOver where
  mempty = GameOver 0
  mappend (GameOver n0) (GameOver n1) = GameOver (max n0 n1)

data Level = Level { floorRects :: ![Rect]
                   , ceilingRects :: ![Rect]
                   , obsticleRects :: ![Rect]}
             deriving (Show)

levelRects :: Level -> [Rect]
levelRects l = floorRects l ++ ceilingRects l ++ obsticleRects l
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
class CanContain a where
  contains :: Point -> a -> Bool
  overlapping :: a -> a -> Bool
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
newtype Point = Point (V2 Double)
                deriving (Show, Eq, Num, Ord, Epsilon)
mkPoint :: Double -> Double -> Point
mkPoint x y = Point (V2 x y)

unPoint :: Point -> (Double, Double)
unPoint (Point (V2 x y)) = (x,y)


data Rect = Rect Point (V2 Double)
          deriving (Show)

mkRect :: Double -> Double -> Double -> Double -> Rect
mkRect x y w h = Rect (mkPoint x y) (V2 w h)

unRect :: Rect -> ((Double, Double), (Double, Double))
unRect (Rect p (V2 w h)) = (unPoint p, (w, h))


instance CanContain Rect where
  contains (Point (V2 x y)) (Rect (Point (V2 x' y')) (V2 w h)) =
    x'    <= x && x <= (x'+w)
    && y' <= y && y <= (y'+h)
  overlapping r0@(Rect (Point (V2 x0 y0)) (V2 w0 h0))
              r1@(Rect (Point (V2 x1 y1)) (V2 w1 h1)) =
                       contains (mkPoint x0 y0) r1 ||
                       contains (mkPoint (x0) (y0+h0)) r1 ||
                       contains (mkPoint (x0+w0) (y0)) r1 ||
                       contains (mkPoint (x0+w0) (y0+h0)) r1 ||
                       contains (mkPoint x1 y1) r0 ||
                       contains (mkPoint (x1) (y1+h1)) r0 ||
                       contains (mkPoint (x1+w1) (y1)) r0 ||
                       contains (mkPoint (x1+w1) (y1+h1)) r0
--------------------------------------------------------------------------------

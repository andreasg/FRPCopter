module Main where

import Level
import Params

import System.Random (getStdGen, RandomGen)
import Linear

import qualified Control.Monad as CM
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLPrim
import Control.Monad.Reader (Reader)
import qualified Control.Monad.Reader as CMR
import Control.Wire
import Prelude hiding (id, (.))
import FRP.Netwire

import Data.Bits
import Data.Word (Word8)

data UserCommand = Accelerate | QuitGame | NoCmd
  deriving (Show, Eq)

parseEvent :: IO UserCommand
parseEvent = do
  event <- SDL.pollEvent
  case event of
    SDL.KeyDown (SDL.Keysym key mods unic)  ->
      case key of
        SDL.SDLK_SPACE -> return Accelerate
        SDL.SDLK_ESCAPE -> return QuitGame
        _              -> return NoCmd
    SDL.Quit -> return QuitGame
    _        -> return NoCmd
      

rgbColor :: Word8 -> Word8 -> Word8 -> SDL.Pixel
rgbColor r g b = SDL.Pixel (shiftL (fi r) 24 .|.
                            shiftL (fi g) 16 .|.
                            shiftL (fi b) 8 .|.
                            255)
  where fi = fromIntegral

white :: SDL.Pixel
white = rgbColor 255 255 255

red :: SDL.Pixel
red = rgbColor 255 0 0

green :: SDL.Pixel
green = rgbColor 0 255 0

main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  screen <- SDL.setVideoMode screenW screenH 32 [SDL.SWSurface]
  
  g <- getStdGen
  go screen clockSession_ (game g)

   
  
  where
    
  go screen s w = do
      cmd <- parseEvent
      (ds, s') <- stepSession s
      
      let (continue, w') = flip CMR.runReader defaultGameParams $
           do
            (eab, w'') <- stepWire w ds (Right cmd)
            case eab of
              Left _ -> return (Nothing, w'')
              Right e -> return (Just e, w'')
      case continue of
        Nothing ->  go screen s' w' 
        Just (Just (x, (Ceiling c, Floor f))) -> do
          clear screen

          SDLPrim.bezier screen (pad $ map (\(x',y) -> (round (x' - x), round y)) c) 100 green
          SDLPrim.bezier screen (pad $ map (\(x',y) -> (round (x' - x), round y)) f) 100 red
--          CM.forM (c) $ \(x',y) ->
--            SDLPrim.circle screen (round (x' - x)) (round y) 4 green


--          CM.forM (f) $ \(x',y) ->
--            SDLPrim.circle screen (round (x' - x)) (round y) 4 red

          SDL.flip screen
          go screen s' w' 
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40 >>= SDL.fillRect s Nothing

pad [] = []
pad (x:[]) = x:[]
pad ((x,y):xs) = (screenW,y):(x,y):xs

game :: (HasTime t s, RandomGen g) => g -> Wire s () (Reader GameParams) UserCommand (Maybe (Double, (Ceiling, Floor)))
game g =
  mkGenN $ \_ -> do
    gp <- CMR.ask
    return (Left (), pure Nothing . onUserCommand QuitGame <|> arr Just . (scrollPos gp  &&& level g))

 where scrollPos gp = (time * (scrollSpeed gp)) >>> arr (realToFrac :: Real a => a -> Double)


data Helicopter = Helicopter (V2 Double) deriving (Show)


position :: (HasTime t s) => Wire s () m Double Double
position = mkPureN $ const (Right 0.0, integral 0.0)
  where
    go x =
      mkPure $ \ds vel ->
                let t = realToFrac . dtime $ ds
                    pos = x+vel*t
                    col = pos < 0 || 150 < pos
                    bounded = if col then max 1 (min 149 pos) else pos
                in (Right bounded, go bounded)
                   
main' :: IO ()
main' = do
       g <- getStdGen
       
       testWireM (return . flip CMR.runReader defaultGameParams) clockSession_ (w g)



  where w :: (HasTime t s, RandomGen g) => g -> Wire s () (Reader GameParams) a (Ceiling, Floor)
        w = level
           




helicopter :: (HasTime t s) => Wire s () (Reader Double) UserCommand Helicopter
helicopter = undefined
{-  
  where
  fly = proc cmd -> do
    thrust <- thrust -< cmd

    pos <- integral (V2 (fromIntegral screenW / 2) (fromIntegral screenH / 2)) -< thrust

    returnA -< Helicopter pos

  thrust = (pure (V2 (-10) 0) . when ((==) Accelerate)) <|> pure 0
-}
-- create a position by using your initial position as
-- the base for integration, and then itegrat the velocity.


onUserCommand :: (Monoid e, Monad m) => UserCommand -> Wire s e m UserCommand a
onUserCommand command =
  mkPure_ $ \cmd -> if cmd == command then Right undefined else Left mempty

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

import qualified Data.Set as Set

data Direction = Up | Down | East | West deriving (Eq, Show, Ord)

data UserCommand = Accelerate Direction | QuitGame
  deriving (Show, Eq, Ord)

parseEvent :: Set.Set UserCommand -> IO (Set.Set UserCommand)
parseEvent active = do
  event <- SDL.pollEvent

  return $ case event of
    SDL.KeyDown (SDL.Keysym key _ _)  ->
      case key of
        SDL.SDLK_SPACE -> Set.insert (Accelerate Up) active
        SDL.SDLK_UP -> Set.insert (Accelerate Up) active
        SDL.SDLK_LEFT -> Set.insert (Accelerate West) active
        SDL.SDLK_RIGHT -> Set.insert (Accelerate East) active
        SDL.SDLK_DOWN -> Set.insert (Accelerate Down) active
        SDL.SDLK_ESCAPE -> Set.insert QuitGame active
        _              -> active
    SDL.KeyUp (SDL.Keysym key _ _)  ->
      case key of
        SDL.SDLK_SPACE -> Set.delete (Accelerate Up) active
        SDL.SDLK_UP -> Set.delete (Accelerate Up) active
        SDL.SDLK_LEFT -> Set.delete (Accelerate West) active
        SDL.SDLK_RIGHT -> Set.delete (Accelerate East) active
        SDL.SDLK_DOWN -> Set.delete (Accelerate Down) active
        SDL.SDLK_ESCAPE -> Set.delete QuitGame active
        _               -> active

    SDL.Quit -> Set.insert QuitGame active
    _        -> active

main = do
       g <- getStdGen
       testWireM (return . flip CMR.runReader defaultGameParams) clockSession_ (ceiling' g)



main' :: IO ()
main' = do
  SDL.init [SDL.InitEverything]
  SDL.setCaption "FRPCopter" ""
  screen <- SDL.setVideoMode screenW screenH 32 [SDL.HWSurface]
  go Set.empty screen clockSession_ . game =<< getStdGen
  SDL.quit
  where
  go cmds' screen s w = do
      green <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 0 255 0 255
      red <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 0 0 255      
      white <- SDL.mapRGBA (SDL.surfaceGetPixelFormat screen) 255 255 255 255      
      cmds <- parseEvent cmds'
      (ds, s') <- stepSession s
      let (continue, w') = flip CMR.runReader defaultGameParams $
           do
            (eab, w'') <- stepWire w ds (Right cmds)
            case eab of
              Left _ -> return (Nothing, w'')
              Right e -> return (Just e, w'')
      case continue of
        Nothing ->  go cmds screen s' w'
        Just Nothing -> putStrLn "quitting"
        Just (Just (x, (o,(c, f)), (V2 px py))) -> do
          clear screen
          SDLPrim.circle screen (round (px - x)) (round py) 10 white
          CM.forM (map (rectToSDLRect x) c) $ flip (SDL.fillRect screen) green . Just
          CM.forM (map (rectToSDLRect x) f) $ flip (SDL.fillRect screen) green . Just
          CM.forM (map (rectToSDLRect x) o) $ flip (SDL.fillRect screen) red . Just

          SDL.flip screen
          go cmds screen s' w'
  clear s = CM.void $ SDL.mapRGB (SDL.surfaceGetPixelFormat s) 40 40 40 >>= SDL.fillRect s Nothing

pad :: Num a => [(a, a)] -> [(a,a)]
pad [] = []
pad (x:[]) = x:[]
pad ((x,y):xs) = (screenW,y):(x,y):xs


rectToSDLRect :: Double -> Rect -> SDL.Rect
rectToSDLRect dx (Rect (V2 x y) (V2 w h)) = SDL.Rect (round (x - dx)) (round y) (round w) (round h)

game :: (HasTime t s, RandomGen g, Fractional t)
     => g
     -> Wire s () (Reader GameParams) (Set.Set UserCommand) (Maybe (Double, ([Rect], ([Rect], [Rect])), V2 Double))
game g =
  mkGenN $ \_ -> do
    gp <- CMR.ask
    return (Left (), ((pure (Just (0, ([], ([],[])), V2 0 0)) . unlessUserCommand (Accelerate Up)) --> (pure Nothing . onUserCommand QuitGame <|> run gp)))
  where scrollPos gp = (time * pure (scrollSpeed gp)) >>> arr (realToFrac :: Real a => a -> Double)
        run gp = proc cmds -> do sp <- scrollPos gp -< ()
                                 (o, (c, l))  <- level g -< ()
                                 p  <- position -< (cmds, 300)
                                 col <- isColliding -< (p, (o ++ c ++ l))
                                 if not col
                                   then returnA -< Just (sp, (o, (c, l)), p)
                                   else returnA -< Nothing

isColliding :: Monad m => Wire s e m (V2 Double, [Rect]) Bool
isColliding = proc (pos, rs) -> do
  returnA -< any (contains pos) rs

onUserCommand :: (Monoid e, Monad m) => UserCommand -> Wire s e m (Set.Set UserCommand) a
onUserCommand command =
  mkPure_ $ \cmd -> if command `Set.member` cmd then Right undefined else Left mempty

unlessUserCommand :: (Monoid e, Monad m) => UserCommand -> Wire s e m (Set.Set UserCommand) a
unlessUserCommand command =
    mkPure_ $ \cmd -> if not (command `Set.member` cmd) then Right undefined else Left mempty

acceleration :: (HasTime t s, Monoid e) => Wire s e (Reader GameParams) (Set.Set UserCommand, Double) (V2 Double)
acceleration = proc (cmd, acc) -> do
  base   <- baseAcceleration -< ()
  up     <- arr (V2 0 . negate . snd) . on Up        <|> neutral -< (cmd, acc)
  down   <- arr (V2 0 . snd) . on Down               <|> neutral -< (cmd, acc)
  east   <- arr (flip V2 0 . snd) . on East          <|> neutral -< (cmd, acc)
  west   <- arr (flip V2 0 . negate . snd) . on West <|> neutral -< (cmd, acc)
  returnA -< foldl1 (^+^) [base,up,down,east,west]
  where neutral = pure $ V2 0 0
        on c = first (onUserCommand (Accelerate c))

gravityVector :: (HasTime t s, Monoid e) => Wire s e (Reader GameParams) (Set.Set UserCommand, Double) (V2 Double)
gravityVector = (integral (V2 0 100) . pure (V2 0 1000) . first (unlessUserCommand (Accelerate Up)))
                -->  pure (V2 0 0) . first (onUserCommand (Accelerate Up))
                --> gravityVector

baseAcceleration :: Monoid e => Wire s e (Reader GameParams) a (V2 Double)
baseAcceleration = mkGen_ $ \_ ->
  do gp <- CMR.ask
     return . Right $ V2 (scrollSpeed gp) 0

position :: (HasTime t s, Monoid e) => Wire s e (Reader GameParams) (Set.Set UserCommand, Double) (V2 Double)
position = integral (V2 100 100) . (arr (uncurry (^+^))) . (gravityVector &&& acceleration)

module Main where

import Linear
import Data.Matrix (Matrix)
import qualified Graphics.UI.SDL as SDL
import Data.Set (Set)
import qualified Data.Set as Set

newtype Coord = Coord { unCoord :: V2 Int }
              deriving (Show, Eq, Ord)

newtype Color = Color { unColor :: SDL.Color }

instance Show Color where
  show (Color (SDL.Color r g b)) = "Color " ++ show (r,g,b)

instance Eq Color where
  (Color (SDL.Color r g b)) == (Color (SDL.Color r' g' b')) =
    r == r' && g == g' && b == b'

data Block = Block Coord Color
           deriving (Show, Eq)

data Shape = I | J | L | O | S | T | Z
           deriving (Show, Eq)

data Figure = Figure Shape (Set Block)
            deriving (Show, Eq)

data Well = Well (Matrix (Maybe Figure))
          deriving (Show)

data Tetris = Tetris {
    activeFigure :: Figure
    , gameState :: GameState
    , well :: Well }
            deriving (Show)

data GameState = Running
               | Paused
               | Stopped
               deriving (Show)

main :: IO ()
main = print "FRPTetris"

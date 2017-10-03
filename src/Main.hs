module Main where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Block
import Board
import HGame
import Mino
import HConsole

-- constants

width, height, offset, fps :: Int
width = 600
height = 600
offset = 10
fps = 15

window :: Display
window = InWindow "HTris" (width, height) (offset, offset)

background :: Color
background = greyN 0.2

render :: HGame -> IO Picture
render game = return $ pictures [(renderBoard b), (renderScorePane 440000)]
  where b = activeBoard game

testEvent :: Event -> HGame -> IO HGame
testEvent (EventKey (SpecialKey KeyLeft) pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just ALeft }
               Up   -> old { button = Nothing }
testEvent (EventKey (SpecialKey KeyRight) pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just ARight }
               Up -> old { button = Nothing }
testEvent (EventKey (SpecialKey KeyDown) pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just ADown }
               Up -> old { button = Nothing }
testEvent (EventKey (SpecialKey KeyUp) pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just AUp, buttonChanged = True }
               Up -> old { button = Nothing }
testEvent (EventKey (Char 'd') pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just AD, buttonChanged = True }
               Up -> old { button = Nothing }
testEvent (EventKey (Char 'a') pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just AA, buttonChanged = True }
               Up -> old { button = Nothing }
testEvent _ old = return old

step :: Float -> HGame -> IO HGame
step seconds old = return $ stepWorld seconds old

main :: IO()
main = do
  ts <- getRandomTypes
  playIO window background fps (initialState ts) render testEvent step 
  
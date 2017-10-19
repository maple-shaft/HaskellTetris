module Main where

import Debug.Trace
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Block
import Board
import HGame
import Mino
import HConsole
import GlossUtilities
import Menu

-- constants

width, height, offset, fps :: Int
width = 600
height = 800
offset = 10
fps = 15

window :: Display
window = InWindow "HTris" (width, height) (offset, offset)

background :: Color
background = greyN 0.2

render :: Picture -> HGame -> IO Picture
render bg game = return $ pictures renderPictures
  where b = activeBoard game
        scorePane = renderScorePane $ score game
        nextBox = renderNextMino $ nextMino game
        holdBox = renderHoldMino $ holdMino game
        levelBox = renderLevelBox $ level game
        menuBox = renderMenu menuPic
        renderPictures = if (started game)
                            then [bg, (renderBoard b), scorePane, nextBox, holdBox, levelBox]
                            else [bg, menuBox]

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
testEvent (EventKey (Char 's') pressed _ _) old =
   return $ case pressed of
               Down -> old { button = Just AS, buttonChanged = True }
               Up -> old { button = Nothing }
testEvent (EventKey (MouseButton LeftButton) Down _ c) old =
   if (started old)
       then return old
       else case (checkClickEvent c) of
               Just StartButton -> return old { started = True }
               _                -> return old
testEvent _ old = return old

step :: Float -> HGame -> IO HGame
step seconds old = if (started old)
                      then return $ stepWorld seconds old
                      else return old

main :: IO()
main = do
  ts <- getRandomTypes
  bg <- backgroundPic
  playIO window background fps (initialState ts) (render bg) testEvent step 


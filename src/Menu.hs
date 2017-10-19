module Menu where

import Block
import Mino
import Board
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import Data.List

menuBackgroundColor = blue
menuBorderColor = white
menuTextColor = white
menuWidth = 350.0
menuHeight = 650.0
menuLocation = (0,0) :: (Float,Float)
menuLabelLocation = (-90,220) :: (Float,Float)
menuTextScale = 0.2

data MenuButtonType = StartButton | ExitButton deriving (Show)
data MenuButton = MenuButton { location :: (Float,Float)
                             , labelLocation :: (Float,Float)
                             , buttonSize :: (Float,Float)
                             , labelText :: String
                             , buttonType :: MenuButtonType
                             } deriving (Show)
                               
startButton = MenuButton { location = (0,40)
                         , labelLocation = (-30,30)
                         , buttonSize = (100,80)
                         , labelText = "Start"
                         , buttonType = StartButton
                         }
                         
exitButton = MenuButton { location = (0,-60)
                        , labelLocation = (-20,-70)
                        , buttonSize = (100,80)
                        , labelText = "Exit"
                        , buttonType = ExitButton
                        }
                        
leftMostCoordinate :: MenuButton -> Float
leftMostCoordinate b = middleX - (width / 2)
  where middleX = fst $ location b
        width = fst $ buttonSize b
        
rightMostCoordinate :: MenuButton -> Float
rightMostCoordinate b = (leftMostCoordinate b) + (fst $ buttonSize b)
        
topMostCoordinate :: MenuButton -> Float
topMostCoordinate b = (bottomMostCoordinate b) + (snd $ buttonSize b)
        
bottomMostCoordinate :: MenuButton -> Float
bottomMostCoordinate b = middleY - (height / 2)
  where middleY = snd $ location b
        height = snd $ buttonSize b
                        
renderButton :: MenuButton -> Picture
renderButton b = pictures [bPic, bText]
  where uncurriedTranslate = uncurry translate
        bPic = color blue $
               uncurry translate (location b) $
               uncurry rectangleSolid $ buttonSize b
        bText = color black $
                uncurry translate (labelLocation b) $
                scale menuTextScale menuTextScale $
                text $ labelText b

renderMenu :: Picture -> Picture
renderMenu menuPic = pictures [menuBox, title, startB, exitB]
  where (mX,mY) = menuLocation
        (tX,tY) = menuLabelLocation
        menuBorder = color black $
                     translate mX mY $
                     rectangleSolid menuWidth menuHeight
        menuBox = translate mX mY $ menuPic
        title = color black $
                translate tX tY $
                scale menuTextScale menuTextScale $
                text "Haskell Tetris"
        startB = renderButton startButton
        exitB = renderButton exitButton
        
checkClickEvent :: (Float,Float) -> Maybe MenuButtonType
checkClickEvent (x', y') = case button of
                              Nothing -> Nothing
                              Just b  -> Just (buttonType b)
  where isX b = elem x' [(leftMostCoordinate b)..(rightMostCoordinate b)]
        isY b = elem y' [(topMostCoordinate b),((topMostCoordinate b) - 1)..(bottomMostCoordinate b)]
        button = find (\b -> (isX b) && (isY b)) [startButton, exitButton]
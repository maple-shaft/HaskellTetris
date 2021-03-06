module Menu where

import GlossUtilities
import Graphics.Gloss
import Data.List

menuBackgroundColor, menuBorderColor, menuTextColor :: Color
menuBackgroundColor = blue
menuBorderColor = white
menuTextColor = white

menuWidth, menuHeight, menuTextScale :: Float
menuWidth = 350.0
menuHeight = 650.0
menuTextScale = 0.4

menuLocation, menuLabelLocation :: (Float,Float)
menuLocation = (0,0)
menuLabelLocation = (10,200)

data MenuButtonType = StartButton | ExitButton deriving (Show)
data MenuButton = MenuButton { location :: (Float,Float)
                             , buttonSize :: (Float,Float)
                             , menuButtonPicture :: Picture
                             , buttonType :: MenuButtonType
                             } deriving (Show)
                        
startButton :: MenuButton       
startButton = MenuButton { location = (0,40)
                         , buttonSize = (100,80)
                         , menuButtonPicture = startButtonPic
                         , buttonType = StartButton
                         }

exitButton :: MenuButton                         
exitButton = MenuButton { location = (0,-60)
                        , buttonSize = (100,80)
                        , menuButtonPicture = exitButtonPic
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
renderButton b = uncurry translate (location b) $ menuButtonPicture b 

renderMenu :: Picture
renderMenu = pictures [menuBox, title, startB, exitB]
  where (mX,mY) = menuLocation
        (tX,tY) = menuLabelLocation
        menuBox = translate mX mY $ menuBgPicture
        title = translate tX tY $
                   scale menuTextScale menuTextScale $
                   logoPic
        startB = renderButton startButton
        exitB = renderButton exitButton
        
checkClickEvent :: (Float,Float) -> Maybe MenuButtonType
checkClickEvent (x', y') = case button of
                              Nothing -> Nothing
                              Just b  -> Just (buttonType b)
  where isX b = elem x' [(leftMostCoordinate b)..(rightMostCoordinate b)]
        isY b = elem y' [(topMostCoordinate b),((topMostCoordinate b) - 1)..(bottomMostCoordinate b)]
        button = find (\b -> (isX b) && (isY b)) [startButton, restartButton, exitButton]
        
restartButton :: MenuButton
restartButton = startButton { location = (0,-200) }

renderRestartButton :: Picture
renderRestartButton = uncurry translate (location restartButton) (menuButtonPicture restartButton)
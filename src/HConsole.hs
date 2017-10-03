module HConsole where

import Block
import Mino
import Board
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
--import Graphics.Rendering.OpenGL                        (($=), get)
--import qualified Graphics.Rendering.OpenGL.GL           as GL
--import qualified Graphics.Rendering.OpenGL.GLU.Errors   as GLU
--import qualified Graphics.UI.GLUT as GLUT

-- constants
scorePaneBackgroundColor = black --makeColorI 0x00 0x00 0x00 0x00
scorePaneBorderColor = makeColorI 0xFF 0xFF 0xFF 0xFF
scorePaneTextColor = makeColorI 0xFF 0xFF 0xFF 0xFF
scorePaneWidth = 200.0
scorePaneInnerWidth = 195.0
scorePaneHeight = 50.0
scorePaneInnerHeight = 45.0
scorePaneTextLocation = (-60,230) :: (Float,Float)
scorePaneBoxLocation = (-10,240) :: (Float,Float)
scoreTextScale = 0.2

renderScorePane :: Int -> Picture
renderScorePane score = pictures [scoreBorderBox, scoreBox, scoreText]
  where (tX,tY) = scorePaneTextLocation
        (bX,bY) = scorePaneBoxLocation
        scoreText = color scorePaneTextColor $
                    translate tX tY $
                    scale scoreTextScale scoreTextScale $
                    text $ show score
        scoreBorderBox = color scorePaneBorderColor $
                         translate bX bY $
                         rectangleSolid scorePaneWidth scorePaneHeight
        scoreBox = color scorePaneBackgroundColor $
                   translate bX bY $
                   rectangleSolid scorePaneInnerWidth scorePaneInnerHeight
                    
--data AltPicture = AltText String
--                     | Nada
--   deriving (Show)
   
--renderAlt :: AltPicture -> IO()
--renderAlt a = case a of
--                 AltText str -> do
--                                     GL.blend $= GL.Disabled
--                                     GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
--                                     GL.blend $= GL.Enabled
--                        Nada -> pure



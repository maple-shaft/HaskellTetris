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
scorePaneLabelLocation = (-60,270) :: (Float,Float)
scoreTextScale = 0.2

nextBoxBackgroundColor = black
nextBoxBorderColor = white
nextTextColor = white
nextBoxWidth = 120.0
nextBoxInnerWidth = 115.0
nextBoxHeight = 80.0
nextBoxInnerHeight = 75.0
nextBlockLocation = (270,180) :: (Float,Float)
nextBoxLocation = (160,150) :: (Float,Float)
nextTextLocation = (150,200) :: (Float,Float)
nextTextScale = 0.15

holdBoxBackgroundColor = black
holdBoxBorderColor = white
holdBoxTextColor = white
holdBoxWidth = 120.0
holdBoxInnerWidth = 115.0
holdBoxHeight = 80.0
holdBoxInnerHeight = 75.0
holdBlockLocation = (-70,180) :: (Float,Float)
holdBoxLocation = (-180,150) :: (Float,Float)
holdTextLocation = (-190,200) :: (Float,Float)
holdTextScale = 0.15

levelBoxBackgroundColor = black
levelBoxBorderColor = white
levelBoxTextColor = white
levelBoxWidth = 100.0
levelBoxInnerWidth = 95.0
levelBoxHeight = 50.0
levelBoxInnerHeight = 45.0
levelBlockLocation = (-60,50) :: (Float,Float)
levelBoxLocation = (-170,20) :: (Float,Float)
levelTextLocation = (-170,15) :: (Float,Float)
levelLabelLocation = (-190,70) :: (Float,Float)
levelTextScale = 0.15

renderScorePane :: Int -> Picture
renderScorePane score = pictures [scoreBorderBox, scoreBox, scoreText, scoreLabel]
  where (tX,tY) = scorePaneTextLocation
        (bX,bY) = scorePaneBoxLocation
        (lX,lY) = scorePaneLabelLocation
        scoreText = color scorePaneTextColor $
                    translate tX tY $
                    scale scoreTextScale scoreTextScale $
                    text $ show score
        scoreLabel = color black $
                     translate lX lY $
                     scale 0.15 0.15 $
                     text "Score"
        scoreBorderBox = color scorePaneBorderColor $
                         translate bX bY $
                         rectangleSolid scorePaneWidth scorePaneHeight
        scoreBox = color scorePaneBackgroundColor $
                   translate bX bY $
                   rectangleSolid scorePaneInnerWidth scorePaneInnerHeight
                   
renderNextMino :: Mino -> Picture
renderNextMino m = pictures [nextBorderBox, nextBox, nextBlocks, nextLabel]
  where (tX,tY) = nextBlockLocation
        (bX,bY) = nextBoxLocation
        (lX,lY) = nextTextLocation
        nextBlocks = pictures $ (renderBlock tX tY) <$> (minoBlocks m)
        nextBorderBox = color nextBoxBorderColor $
                        translate bX bY $
                        rectangleSolid nextBoxWidth nextBoxHeight
        nextBox = color nextBoxBackgroundColor $
                  translate bX bY $
                  rectangleSolid nextBoxInnerWidth nextBoxInnerHeight
        nextLabel = color black $
                    translate lX lY $
                    scale nextTextScale nextTextScale $
                    text $ "Next"
                    
renderHoldMino :: Maybe Mino -> Picture
renderHoldMino m = pictures [holdBorderBox, holdBox, holdBlocks, holdLabel]
  where (tX,tY) = holdBlockLocation
        (bX,bY) = holdBoxLocation
        (lX,lY) = holdTextLocation
        holdBlocks = case m of
                         Nothing   -> pictures []
                         Just mino -> pictures $ (renderBlock tX tY) <$> (minoBlocks mino)
        holdBorderBox = color holdBoxBorderColor $
                        translate bX bY $
                        rectangleSolid holdBoxWidth holdBoxHeight
        holdBox = color holdBoxBackgroundColor $
                  translate bX bY $
                  rectangleSolid holdBoxInnerWidth holdBoxInnerHeight
        holdLabel = color black $
                    translate lX lY $
                    scale holdTextScale holdTextScale $
                    text $ "Hold"
                    
renderLevelBox :: Int -> Picture
renderLevelBox l = pictures [levelBorderBox, levelBox, levelText, levelLabel]
  where (tX,tY) = levelTextLocation
        (bX,bY) = levelBoxLocation
        (lX,lY) = levelLabelLocation
        levelText = color levelBoxTextColor $
                    translate tX tY $
                    scale levelTextScale levelTextScale $
                    text $ show l
        levelBorderBox = color levelBoxBorderColor $
                        translate bX bY $
                        rectangleSolid levelBoxWidth levelBoxHeight
        levelBox = color levelBoxBackgroundColor $
                  translate bX bY $
                  rectangleSolid levelBoxInnerWidth levelBoxInnerHeight
        levelLabel = color black $
                    translate lX lY $
                    scale levelTextScale levelTextScale $
                    text $ "Level"
                    
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
{-# LANGUAGE ScopedTypeVariables #-}
module Block where

import Control.Monad
import System.Random
import qualified Data.Map (Map, fromList) 
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

-- constants

size, innerBSide :: Float
size = 20
innerBSide = 18

blockBorderColor = white
boardBackground = makeColorI 0xF0 0xF0 0xF0 0xF0
boardBorderColor = makeColorI 0xFA 0xFA 0xFA 0xFA
settledBlockColor = greyN 0.2
   
data Action = AUp | ADown | ALeft | ARight | AA | AS | AD
       deriving (Show, Eq)

data BlockType = Background | Active | Settled | Hint
       deriving (Show, Eq)

data Block = Block
   { blockType :: BlockType
   , coordinate :: (Int, Int)
   , blockColor :: Color
   } deriving (Show)
   
instance Eq Block where
   x == y = a && b
     where a = (blockType x) == (blockType y)
           b = (coordinate x) == (coordinate y)

-- | Order Blocks by the natural ordering of their coordinates.  This helps line up
-- | blocks to state delta coordinates for zipping when performing a rotation.
instance Ord Block where
   x `compare` y = compare (coordinate x) (coordinate y)
                    
makeActiveBlock :: Color -> (Int, Int) -> Block
makeActiveBlock c (x,y) = Block { blockType = Active
                              , coordinate = (x,y)
                              , blockColor = c
                              }   

makeSettledBlock :: (Int, Int) -> Block
makeSettledBlock (x,y) = Block { blockType  = Settled
                             , coordinate = (x,y)
                             , blockColor = settledBlockColor
                             }
                             
makeBackgroundBlock :: Int -> Int -> Block
makeBackgroundBlock x y = Block
                         { blockType  = Background
                         , blockColor = boardBackground
                         , coordinate = (x,y)
                         }
                         
makeHintBlock :: Color -> (Int,Int) -> Block
makeHintBlock c coor = Block
                         { blockType = Hint
                         , blockColor = c
                         , coordinate = coor
                         }

renderHint :: Float -> Float -> [Block] -> Picture
renderHint xOffset yOffset h = pictures $ (renderBlock xOffset yOffset) <$> h

renderBlock :: Float -> Float -> Block -> Picture
renderBlock xOffset yOffset b = pictures [borderBlock, innerBlock]
  where innerBlockColor = case (blockType b) of
                             Hint -> color boardBackground
                             _       -> color (blockColor b)
        innerBlock = translate (xPixel) (yPixel) $ innerBlockColor $ rectangleSolid innerBSide innerBSide
        borderBlockColor = case (blockType b) of
                              Background -> color boardBorderColor
                              Hint    -> color $ blockColor b
                              _          -> color blockBorderColor
        borderBlock = translate xPixel yPixel $ borderBlockColor $ rectangleSolid size size
        (xPixel,yPixel) = convertCoordinate xOffset yOffset (coordinate b)
            
convertCoordinate :: Float -> Float -> (Int,Int) -> (Float,Float)
convertCoordinate xOffset yOffset (x,y) = (xPixel, yPixel)
  where xPixel = ((fromIntegral (-x)) * 20.0) + xOffset
        yPixel = ((fromIntegral (-y)) * 20.0) + yOffset
        
        -- | Takes a delta coordinate and moves block by delta, no collision detection is occurring in this method.
-- | The assumption is that collision detection has already been vetted.
moveBlock :: (Int, Int) -> (Int,Int) -> Block -> Block
moveBlock (x,y) (deltaX,deltaY) b = 
     b { coordinate = (potentialX,potentialY) }
  where (potentialX,potentialY) = ((x + deltaX),(y + deltaY))
  
moveBlockAbsolute :: (Int,Int) -> Block -> Block
moveBlockAbsolute c b = b { coordinate = c }
  
moveBlockFlipped :: Block -> (Int,Int) -> (Int,Int) -> Block
moveBlockFlipped a b c = moveBlock b c a
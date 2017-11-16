{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Block where

import Data.Map as M
import Data.List as L
import Data.Maybe (fromJust)
import Debug.Trace
import Graphics.Gloss
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T (pack, unpack, split, Text)
import GHC.Generics

-- constants

bSize, innerBSide :: Float
bSize = 20
innerBSide = 18

blockBorderColor, boardBackground, boardBorderColor, settledBlockColor :: Color
blockBorderColor = white
boardBackground = makeColorI 0xF0 0xF0 0xF0 0xF0
boardBorderColor = makeColorI 0xFA 0xFA 0xFA 0xFA
settledBlockColor = greyN 0.2
   
data Action = AUp | ADown | ALeft | ARight | AA | AS | AD
       deriving (Show, Eq, Generic)
       
instance ToJSON Action where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Action

data BlockType = Background | Active | Settled | Hint
       deriving (Show, Eq, Generic)
       
instance ToJSON BlockType where
   toEncoding = genericToEncoding defaultOptions
   
instance FromJSON BlockType where
  parseJSON = genericParseJSON defaultOptions
   
instance ToJSON Color where
  toJSON c = String (T.pack $ show c)
  
instance FromJSON Color where
  parseJSON = withText "Color" $ textToColor
    where spl :: T.Text -> [Float]
          spl t = read . (T.unpack) <$> ( tail $ T.split (\c -> c == ' ') t)
          colr [] = Nothing
          colr (r:g:b:a:_) = Just (makeColor r g b a)
          textToColor :: T.Text -> Parser Color
          textToColor t = return $ fromJust $ colr (spl t) 

data Block = Block
   { blockType :: BlockType
   , coordinate :: (Int, Int)
   , blockColor :: Color
   } deriving (Generic)
   
instance ToJSON Block where
   toEncoding = genericToEncoding defaultOptions
   
instance FromJSON Block   

instance Eq Block where
   x == y = a && b
     where a = (blockType x) == (blockType y)
           b = (coordinate x) == (coordinate y)

-- | Order Blocks by the natural ordering of their coordinates.  This helps line up
-- | blocks to state delta coordinates for zipping when performing a rotation.
instance Ord Block where
   x `compare` y = compare (coordinate x) (coordinate y)
   
instance Show Block where
   show x = show $ coordinate x
                    
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

-- | Renders a block by a given offset and a possible color.  If the line is currently
-- | being cleared then this color might be a transitional state for animation.
renderBlock :: Float -> Float -> Block -> Picture
renderBlock xOffset yOffset b = pictures [borderBlock, innerBlock]
  where innerBlockColor = case (blockType b) of
                             Hint -> color boardBackground
                             _    -> color (blockColor b)
        innerBlock = translate (xPixel) (yPixel) $ innerBlockColor $ rectangleSolid innerBSide innerBSide
        borderBlockColor = case (blockType b) of
                              Background -> color boardBorderColor
                              Hint    -> color $ blockColor b
                              _       -> color blockBorderColor
        borderBlock = translate xPixel yPixel $ borderBlockColor $ rectangleSolid bSize bSize
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

moveBlockDown :: Block -> Block
moveBlockDown b = b { coordinate = (x,y+1) }
  where (x,y) = coordinate b
  
moveBlockDownI :: Int -> Block -> Block
moveBlockDownI i b = b { coordinate = (x,y+i) }
  where (x,y) = coordinate b
  
moveBlocksDown :: Map (Int,Int) Block -> Map (Int,Int) Block
moveBlocksDown blocks = trace (show f) $ f --M.map moveBlockDown blocks
  where movedBlocks = L.map moveBlockDown (M.elems blocks)
        f = L.foldr (\x acc -> M.insert (coordinate x) x acc) M.empty movedBlocks
        
moveBlocksDownI :: Int -> Map (Int,Int) Block -> Map (Int,Int) Block
moveBlocksDownI i blocks = trace (show f) $ f --M.map moveBlockDown blocks
  where movedBlocks = L.map (moveBlockDownI i) (M.elems blocks)
        f = L.foldr (\x acc -> M.insert (coordinate x) x acc) M.empty movedBlocks
        

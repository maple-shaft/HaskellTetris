{-# LANGUAGE DeriveGeneric #-}
module Mino where

import Block
import Control.Monad
import System.Random
import Data.Maybe
import Data.List as L
import Data.Map as M
import Graphics.Gloss
import GHC.Generics
import Data.Aeson

-- starting coodinates for all 7 types
icoord, ocoord, tcoord, jcoord, lcoord, scoord, zcoord :: [(Int,Int)]
icoord = [(4,2),(5,2),(6,2),(7,2)]
ocoord = [(5,1),(6,1),(5,2),(6,2)]
tcoord = [(5,1),(4,2),(5,2),(6,2)]
jcoord = [(4,1),(4,2),(5,2),(6,2)]
lcoord = [(6,1),(4,2),(5,2),(6,2)]
scoord = [(5,1),(6,1),(4,2),(5,2)]
zcoord = [(4,1),(5,1),(5,2),(6,2)]

-- | States of deltas from mino top left coordinate
states :: M.Map MinoType [[(Int,Int)]]
states = M.fromList $ [
     (I,
       [[(0,1),(1,1),(2,1),(3,1)] -- State 0
       ,[(2,0),(2,1),(2,2),(2,3)] -- State 1
       ,[(0,2),(1,2),(2,2),(3,2)] -- State 2
       ,[(1,0),(1,1),(1,2),(1,3)] -- State 3
       ]
     ),
     (O,
       [[(1,0),(2,0),(1,1),(2,1)] -- State 0
       ,[(1,0),(2,0),(1,1),(2,1)] -- State 1
       ,[(1,0),(2,0),(1,1),(2,1)] -- State 2
       ,[(1,0),(2,0),(1,1),(2,1)] -- State 3
       ]
     ),
     (T,
       [[(1,0),(0,1),(1,1),(2,1)] -- State 0
       ,[(1,0),(1,1),(1,2),(2,1)] -- State 1
       ,[(0,1),(1,1),(1,2),(2,1)] -- State 2
       ,[(0,1),(1,0),(1,1),(1,2)] -- State 3
       ]
     ),
     (J,
       [[(0,0),(0,1),(1,1),(2,1)] -- State 0
       ,[(1,0),(1,1),(1,2),(2,0)] -- State 1
       ,[(0,1),(1,1),(2,1),(2,2)] -- State 2
       ,[(0,2),(1,0),(1,1),(1,2)] -- State 3
       ]
     ),
     (L,
       [[(0,1),(1,1),(2,0),(2,1)] -- State 0
       ,[(1,0),(1,1),(1,2),(2,2)] -- State 1
       ,[(0,1),(0,2),(1,1),(2,1)] -- State 2
       ,[(0,0),(1,0),(1,1),(1,2)] -- State 3
       ]
     ),
     (S,
       [[(1,0),(2,0),(0,1),(1,1)] -- State 0
       ,[(1,0),(1,1),(2,1),(2,2)] -- State 1
       ,[(0,2),(1,1),(1,2),(2,1)] -- State 2
       ,[(0,0),(0,1),(1,1),(1,2)] -- State 3
       ]
     ),
     (Z,
       [[(0,0),(1,0),(1,1),(2,1)] -- State 0
       ,[(1,1),(1,2),(2,0),(2,1)] -- State 1
       ,[(0,1),(1,1),(1,2),(2,2)] -- State 2
       ,[(1,0),(1,1),(0,1),(0,2)] -- State 3
       ]
     )
   ]
   
data Rotation = Clockwise | CounterClockwise
       deriving (Show, Eq)
       
type MinoState = Int

data MinoType = I | O | T | J | L | S | Z
       deriving (Show, Eq, Ord, Generic)
       
instance ToJSON MinoType where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON MinoType

data Mino = Mino
   { minoType :: MinoType
   , minoBlocks :: [Block]
   , minoState :: MinoState
   , minoLocation :: (Int,Int)
   } deriving (Show, Generic)
   
instance ToJSON Mino where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Mino
   
instance Eq Mino where
   x == y = typesEqual && blocksEqual
     where typesEqual = (minoType x) == (minoType y)
           blocksEqual = (minoBlocks x) == (minoBlocks y)
           
-- | This can be custom by level later, for now tie a single color to a type
getMinoColor :: MinoType -> Color
getMinoColor t = case t of
                    I -> cyan
                    O -> makeColorI 0xE0 0xE0 0x0 0xE0
                    T -> magenta
                    J -> blue
                    L -> orange
                    S -> green
                    Z -> red
                    
getRandomTypes :: IO [MinoType]
getRandomTypes = L.map intToType <$> liftM (randomRs (0 :: Int, 6 :: Int)) getStdGen
  where intToType c
             | c == 0 = I
             | c == 1 = O
             | c == 2 = T
             | c == 3 = J
             | c == 4 = L
             | c == 5 = S
             | otherwise = Z

makeMino :: MinoType -> Mino
makeMino t = 
  let startingState = ((2 :: Int) ^ (14 :: Int)) :: MinoState
      make = (makeActiveBlock $ getMinoColor t)
      create x = Mino { minoBlocks = (make <$> x), minoType = t, minoState = startingState, minoLocation = (4,1)}
  in case t of
    I -> create icoord
    O -> create ocoord
    T -> create tcoord
    J -> create jcoord
    L -> create lcoord
    S -> create scoord
    Z -> create zcoord

makeHintBlocks :: Mino -> [Block]
makeHintBlocks m = L.map (makeHintBlock hintColor) coordinates
  where coordinates = coordinate <$> (minoBlocks m)
        hintColor = blockColor ((minoBlocks m) !! 1)
        
-- | Takes a delta coordinate and moves mino by delta.  No collision detection is occurring in this method.
moveMino :: Mino -> (Int,Int) -> Mino
moveMino m delta@(deltaX,deltaY) = m { minoBlocks = movedBlocks, minoLocation = newLoc }
  where blocks = L.sort $ minoBlocks m
        blockCoordinates = L.sort $ L.map coordinate blocks
        movedBlocks = zipWith (\x y -> moveBlock x delta y) blockCoordinates blocks
        (mX,mY) = minoLocation m
        newLoc = ((mX + deltaX),(mY + deltaY))
        
minoBlockDeltaCoordinates :: Mino -> Int -> [(Int,Int)]
minoBlockDeltaCoordinates m rotation = (cycle $ fromJust $ (M.lookup mType states)) !! (mState + rotation)
  where mState = minoState m
        mType = minoType m
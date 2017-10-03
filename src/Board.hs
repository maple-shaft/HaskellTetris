module Board where

import Debug.Trace
import Data.Maybe
import Data.List as L
import Data.Map as M
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Block
import Mino
  
data Board = Board
  { topLeft :: (Float, Float)
  , boardBlocks :: Map (Int, Int) Block
  , settledBlocks :: Map (Int, Int) Block
  , hintBlocks :: Map (Int, Int) Block
  , activeMino :: Mino
  } deriving (Show)

--constants
boardHeight = 22
boardWidth = 10
startYOffset = 200
startXOffset = 100

allBackgroundBlocks = makeBackgroundBlock <$> [1..boardWidth] <*> [1..boardHeight]

startingBoard :: MinoType -> Board
startingBoard t = Board
                  { topLeft = (0,0)
                  , boardBlocks = M.fromList $ (L.map (\x-> ((coordinate x), x)) allBackgroundBlocks)
                  , settledBlocks = M.empty
                  , hintBlocks = M.empty
                  , activeMino = makeMino t
                  }

getBlockAt :: (Int, Int) -> Board -> Maybe Block
getBlockAt (x,y) b = lookupSettled
  where settledB = settledBlocks b
        lookupSettled = M.lookup (x,y) settledB
        
getAllHintBlocks :: Board -> [Block]
getAllHintBlocks b =
   M.elems $ hintBlocks b

getAllBoardBlocks :: Board -> [Block]
getAllBoardBlocks b = M.elems $ boardBlocks b

getAllSettledBlocks :: Board -> [Block]
getAllSettledBlocks b = M.elems $ settledBlocks b

renderBoard :: Board -> Picture
renderBoard b = pictures $ (renderBlock startXOffset startYOffset) <$> allRenders
  where allRenders = L.concat $ L.map ($ b) [getAllBoardBlocks, getAllSettledBlocks, getAllHintBlocks, minoBlocks.activeMino]        

couldMinoMoveDown :: Mino -> Board -> Bool
couldMinoMoveDown m b = all (couldBlockMoveDown b) blocks
  where blocks = minoBlocks m

couldBlockMoveDown :: Board -> Block -> Bool
couldBlockMoveDown board b = 
  let (x,y) = coordinate b
  in (not $ y == boardHeight) && ((getBlockAt (x, y+1) board) == Nothing)

-- | Checks for collisions of a block moving by a delta from a starting coordinate
couldBlockMoveDelta :: Board -> (Int,Int) -> (Int,Int) -> Bool
couldBlockMoveDelta board (x,y) (deltaX,deltaY) = xBool && yBool
  where (potentialX, potentialY) = ((x + deltaX),(y + deltaY))
        noBlockAt = ((getBlockAt (potentialX,potentialY) board) == Nothing)
        xBool = (elem potentialX [1..boardWidth]) && noBlockAt
        yBool = (elem potentialY [1..boardHeight]) && noBlockAt 
        
couldMinoMoveDelta :: Board -> Mino -> (Int,Int) -> Bool
couldMinoMoveDelta b m coor@(cX,cY) = 
     all (\x -> couldBlockMoveDelta b x coor) c
  where c = sort $ L.map coordinate (minoBlocks m)      

findMinoBottom :: Board -> Mino -> (Int,Int)
findMinoBottom b m = (0,a-1)
  where a = fromJust $ L.find (\yDelta -> not $ couldMinoMoveDelta b m (0,yDelta)) [1..]
        
insertBlocksToSettled :: Board -> [Block] -> Map (Int,Int) Block
insertBlocksToSettled b blocks = L.foldr insertSettledBlock (settledBlocks b) blocks
  where insertSettledBlock = (\x acc -> M.insert (coordinate x) x acc)
  
insertBlocksToHint :: Board -> [Block] -> Map (Int,Int) Block
insertBlocksToHint b blocks = L.foldr insertHintBlock M.empty blocks
  where insertHintBlock = (\x acc -> M.insert (coordinate x) x acc)
{-# LANGUAGE DeriveGeneric #-}
module Board where

import Data.Maybe
import Data.List as L
import Data.Map as M
import Graphics.Gloss
import Block
import Mino
import qualified Data.Text as T (pack)
import Data.Aeson
import GHC.Generics

data Player = Player | Opponent 
  deriving (Show, Eq)

data Board = Board
  { topLeft :: (Float, Float)
  , boardBlocks :: Map (Int, Int) Block
  , settledBlocks :: Map (Int, Int) Block
  , hintBlocks :: Map (Int, Int) Block
  , activeMino :: Mino
  , clearingTime :: Float
  , clearCycled :: Bool
  } deriving (Show, Generic)
  
instance ToJSON Board where
  toJSON (Board _ _ sb hb am ct cc) =
    object [ (T.pack "settledBlocks") .= sb
           , (T.pack "hintBlocks") .= hb
           , (T.pack "activeMino") .= am
           , (T.pack "clearingTime") .= ct
           , (T.pack "clearCycled") .= cc
           ]
  --toEncoding b = foldable [(toJSON b)]
  
instance FromJSON Board where
  parseJSON = withObject "Board" objectToBoard
    where objectToBoard v = Board 
                              <$> v .:? (T.pack "topLeft") .!= (0,0)
                              <*> v .:? (T.pack "boardBlocks") .!= createBackgroundBlocks
                              <*> v .: (T.pack "settledBlocks")
                              <*> v .: (T.pack "hintBlocks")
                              <*> v .: (T.pack "activeMino")
                              <*> v .: (T.pack "clearingTime")
                              <*> v .: (T.pack "clearCycled")

--constants
boardHeight, boardWidth :: Int
boardHeight = 22
boardWidth = 10

startYOffset, startXOffset :: Float
startYOffset = 200
startXOffset = 100

allBackgroundBlocks :: [Block]
allBackgroundBlocks = makeBackgroundBlock <$> [1..boardWidth] <*> [1..boardHeight]

createBackgroundBlocks :: Map (Int, Int) Block
createBackgroundBlocks = M.fromList $ (L.map (\x-> ((coordinate x), x)) allBackgroundBlocks)

startingBoard :: MinoType -> Board
startingBoard t = Board
                  { topLeft = (0,0)
                  , boardBlocks = createBackgroundBlocks
                  , settledBlocks = M.empty
                  , hintBlocks = M.empty
                  , activeMino = makeMino t
                  , clearingTime = 0
                  , clearCycled = False
                  }
                  
updateClearTimer :: Float -> Board -> Board
updateClearTimer seconds b = --trace (show (shouldAnimateClearLine potentialBoard)) $
    new
  where startTime = clearingTime b
        potentialBoard = b { clearingTime = (startTime + seconds) }
        areThereLines = not $ ((findNonCompleteLineIndices b) == [])
        new = if (areThereLines)
                  then potentialBoard
                  else b
                  
shouldAnimateClearLine :: Board -> Bool
shouldAnimateClearLine b = --trace (show isTimeUp) $
    (not isTimeUp)
  where isTimeUp = (clearingTime b) > (0.30)
  
shouldClearLineNow :: Board -> Bool
shouldClearLineNow b = (areThereCompleteLines && (not (shouldAnimateClearLine b)))
  where areThereCompleteLines = not ((findNonCompleteLineIndices b) == []) 
        

getBlockAt :: (Int, Int) -> Board -> Maybe Block
getBlockAt (x,y) b = lookupSettled
  where settledB = settledBlocks b
        lookupSettled = M.lookup (x,y) settledB
        
getAllHintBlocks :: Board -> [Block]
getAllHintBlocks b =
   M.elems $ hintBlocks b

getAllBoardBlocks :: Board -> [Block]
getAllBoardBlocks b = M.elems $ boardBlocks b

-- | Get all settled blocks
getAllSettledBlocks :: Board -> [Block]
getAllSettledBlocks b = blocks
  where blocks = M.elems $ settledBlocks b

renderBoard :: Board -> Picture
renderBoard b = pictures $ renderBlockFunc <$> allRenders
  where allRenders = (L.concat $ L.map ($ b) [getAllBoardBlocks, getAllSettledBlocks, getAllHintBlocks, minoBlocks.activeMino])
        renderBlockFunc = renderBlock startXOffset startYOffset

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
couldMinoMoveDelta b m coor = 
     all (\x -> couldBlockMoveDelta b x coor) c
  where c = sort $ L.map coordinate (minoBlocks m)      

findMinoBottom :: Board -> Mino -> (Int,Int)
findMinoBottom b m = (0,a-1)
  where a = fromJust $ L.find (\yDelta -> not $ couldMinoMoveDelta b m (0,yDelta)) [1..]
        
insertBlocksToSettled :: Board -> [Block] -> Map (Int,Int) Block
insertBlocksToSettled b blocks = L.foldr insertSettledBlock (settledBlocks b) blocks
  where insertSettledBlock = (\x acc -> M.insert (coordinate x) x acc)
  
insertBlocksToHint :: [Block] -> Map (Int,Int) Block
insertBlocksToHint blocks = L.foldr insertHintBlock M.empty blocks
  where insertHintBlock = (\x acc -> M.insert (coordinate x) x acc)

getSettledLineBlocks :: Board -> Int -> [Block]
getSettledLineBlocks b f = 
     M.elems maybeBlocks
  where tr = (\k _ -> ((snd k) == f))
        maybeBlocks = filterWithKey tr (settledBlocks b)

findCompleteLineIndices :: Board -> [Int]
findCompleteLineIndices b = L.filter (\x -> not $ (length $ getSettledLineBlocks b x) == boardWidth) [1..boardHeight]

findNonCompleteLineIndices :: Board -> [Int]
findNonCompleteLineIndices b = L.filter (\x -> (length $ getSettledLineBlocks b x) == boardWidth) [1..boardHeight]

findNonCompleteLines :: Board -> [[Block]]
findNonCompleteLines b = L.map (getSettledLineBlocks b) (findCompleteLineIndices b)

findCompleteLines :: Board -> [[Block]]
findCompleteLines b = L.map (getSettledLineBlocks b) (findNonCompleteLineIndices b)

clearCompLines :: Board -> Board
clearCompLines b = if settledBlocksToClear == []
                      then b
                      else b { settledBlocks = newSettledBlocks2 }
  where settledBlocksToClear = L.concat $ findNonCompleteLines b
        newSettledBlocks = M.filter (\x -> elem x settledBlocksToClear) (settledBlocks b)
        boardWithClearedBlocks = b { settledBlocks = newSettledBlocks }
        completedLineIndices = findNonCompleteLineIndices b
        c = M.fromList $ L.map (\x -> (x, length(L.filter (x <) completedLineIndices))) [1..boardHeight]
        f = M.elems $ M.mapWithKey (\(_,ky) v -> moveBlockDownI (fromJust $ M.lookup ky c) v) newSettledBlocks
        finalBoard = boardWithClearedBlocks { settledBlocks = M.empty }
        newSettledBlocks2 = insertBlocksToSettled finalBoard f

newBoardWithActiveMino :: Mino -> Board -> Board
newBoardWithActiveMino m b = b { activeMino = m, hintBlocks = newHint }
  where newHintBlocks = makeHintBlocks m
        newHint = insertBlocksToHint newHintBlocks
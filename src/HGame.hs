module HGame where

import Board
import Block
import Mino
import Debug.Trace
import Data.Maybe
import Data.List as L
import Data.Map as M
import Graphics.Gloss
import Graphics.Gloss.Data.Color

data HGame = Game
  { activeBoard :: Board
  , nextMino :: Mino
  , level :: Int
  , holdMino :: Maybe Mino
  , speed :: Float
  , button :: Maybe Action
  , buttonChanged :: Bool
  , timerCycled :: Bool
  , lastBlockRefresh :: Float
  , score :: Int
  , randomTypes :: [MinoType]
  , started :: Bool
  }
  
-- | Initialize the game with this game state.
initialState :: [MinoType] -> HGame
initialState ts = Game
  { activeBoard = startingBoard $ (head $ tail ts)
  , nextMino = makeMino $ head ts
  , holdMino = Nothing
  , level = 1
  , speed = 1
  , button = Nothing
  , buttonChanged = False
  , timerCycled = False
  , lastBlockRefresh = 0
  , score = 0
  , randomTypes = tail $ tail ts
  , started = False
  }
  
-- Line Score Constants
lineScore :: Int -> Int
lineScore 0 = 0
lineScore 1 = 200
lineScore 2 = 500
lineScore 3 = 1500
lineScore 4 = 6000
lineScore _ = error "Invalid"
  
-- | General step World function.  Master function that calls other game state modifying functions
stepWorld :: Float -> HGame -> HGame
stepWorld seconds old = n5
  where m = activeMino $ activeBoard old
        action = button old
        n1 = updateTimer seconds old
        n2 = n1 { activeBoard = (updateClearTimer seconds (activeBoard old)) }
        allActions = L.map ($ action) [moveMinoToBottomAndStop, moveAction, rotateAction, holdAction, hintUpdate]
        n3 = L.foldl (flip (.)) id allActions $ n2
        n4 = stepMoveDown n3
        n5 = clearLinesState n4
        
updateTimer :: Float -> HGame -> HGame
updateTimer seconds old = new
  where startTime = lastBlockRefresh old
        shouldMove = (startTime + seconds) > (speed old)
        new = if shouldMove
                  then old { lastBlockRefresh = 0, timerCycled = True }
                  else old { lastBlockRefresh = (startTime + seconds), timerCycled = False }
                  
stepMoveDown :: HGame -> HGame
stepMoveDown old = new'
  where b = activeBoard old
        m = activeMino b
        shouldMove = timerCycled old
        couldMove = couldMinoMoveDown m (activeBoard old)
        new = if (shouldMove && (not couldMove))
                 then stopMino old
                 else old
        new' = if (shouldMove && couldMove)
                  then moveDown $ new
                  else new
                  
moveAction :: Maybe Action -> HGame -> HGame
moveAction action old = 
    if (action == Nothing || (not cMovedBlocks))
       then old
       else old { activeBoard = b { activeMino = newM } }
  where b = activeBoard old
        m = activeMino b
        (mX,mY) = minoLocation m
        (deltaX, deltaY) = case action of
                              Just ALeft -> (1,0)
                              Just ARight -> (-1,0)
                              Just ADown -> (0,1)
                              _ -> (0,0)
        cMovedBlocks = all (\a -> couldBlockMoveDelta b (coordinate a) (deltaX,deltaY)) (minoBlocks m)
        newM = moveMino m (deltaX,deltaY)
        
rotateAction :: Maybe Action -> HGame -> HGame
rotateAction action old =
    if ((not cMovedBlocks) || (rotate == 0) || (not $ buttonChanged old))
       then old
       else old { buttonChanged = False
                , activeBoard = b
                                  { activeMino = m
                                                   { minoBlocks = movedBlocks
                                                   , minoState = ((minoState m) + rotate)
                                                   }
                                  }
                }
  where b = activeBoard old
        m = activeMino b
        loc@(mX,mY) = minoLocation m
        blocks = sort $ minoBlocks m
        rotate = case action of
                              Just AA -> -1
                              Just AD -> 1
                              _ -> 0
        c = L.sort $ minoBlockDeltaCoordinates m rotate
        cMovedBlocks = and $ (couldBlockMoveDelta b loc) <$> c
        movedBlocks = zipWith (moveBlock loc) c blocks
        
holdAction :: Maybe Action -> HGame -> HGame
holdAction action old = if action == Just AS && (buttonChanged old)
                           then old { holdMino = Just newHoldMino
                                    , nextMino = newNextMino
                                    , activeBoard = newBoard
                                    , buttonChanged = False
                                    }
                           else old
  where newType = head $ randomTypes old
        newActiveMino = case (holdMino old) of
                            Nothing -> nextMino old
                            Just m  -> m
        newHoldMino = makeMino $ minoType $ activeMino $ activeBoard old
        newBoard = newBoardWithActiveMino newActiveMino (activeBoard old)
        newNextMino = case (holdMino old) of
                            Nothing -> makeMino newType
                            _       -> nextMino old

hintUpdate :: Maybe Action -> HGame -> HGame
hintUpdate action old = 
   if hintBlocks b == M.empty && lastBlockRefresh old == 0
      then old { activeBoard = b { hintBlocks = ( M.fromList $ (L.map (\x-> ((coordinate x), x)) (makeHintBlocks m))) } }
      else if action == Nothing && not (lastBlockRefresh old == 0)
      then old
      else old { activeBoard = b { hintBlocks = newHintBlocks } }
  where b = activeBoard old
        m = activeMino b
        (_,minoBottom) = findMinoBottom b m
        hB = sort $ getAllHintBlocks b
        mC = sort $ (coordinate) <$> (minoBlocks m)
        newHintBlocks = insertBlocksToHint b (zipWith (\(mX,mY) hh -> moveBlockAbsolute (mX, mY + minoBottom) hh) mC hB)
        
-- | Start all animation to clear the lines then get rid of them
clearLinesState :: HGame -> HGame
clearLinesState old = 
    old { activeBoard = newBoard, score = newScore, level = newLevel, speed = newSpeed }
  where b = activeBoard old
        shouldClear = shouldClearLineNow b
        newBoard = if (shouldClear)
                      then (clearCompLines b) { clearingTime = 0 }
                      else b { settledBlocks = newAnimatedSettledBlocks }
        newScore = if (shouldClear)
                      then (lineScore $ (length clearableBlocks) `quot` 10) + score old
                      else score old
        newLevel = (newScore `div` 5000) + 1
        newSpeed = (1 / (fromIntegral newLevel)) :: Float
        clearableBlocks = L.concat $ findCompleteLines b
        brightenedBlocks = L.map (\x -> x { blockColor = light $ blockColor x }) clearableBlocks
        newAnimatedSettledBlocks = L.foldr (\x acc -> M.insert (coordinate x) x acc) (settledBlocks b) brightenedBlocks

moveDown :: HGame -> HGame
moveDown g = moveAction (Just ADown) g

moveMinoToBottomAndStop :: Maybe Action -> HGame -> HGame
moveMinoToBottomAndStop action old =
     if action == Just AUp && (buttonChanged old)
        then stopMino old { activeBoard = b { activeMino = newM }, buttonChanged = False }
        else old
  where b = activeBoard old
        m = activeMino b
        delta = findMinoBottom b m
        newM = moveMino m delta
        
stopMino :: HGame -> HGame
stopMino old =
    ( old { activeBoard = newBoard { settledBlocks = newSettled }
                   , randomTypes = tail $ randomTypes old
                   , nextMino = newNextMino
                   }
               )
  where b = activeBoard old
        mino = activeMino b
        newType = head $ randomTypes old
        newSettled = insertBlocksToSettled b (minoBlocks mino)
        newBoard = newBoardWithActiveMino (nextMino old) b
        newNextMino = makeMino newType

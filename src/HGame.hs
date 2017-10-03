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
  , speed :: Float
  , button :: Maybe Action
  , buttonChanged :: Bool
  , lastBlockRefresh :: Float
  , randomTypes :: [MinoType]
  }
  
-- | Initialize the game with this game state.
initialState :: [MinoType] -> HGame
initialState ts = Game
  { activeBoard = startingBoard $ (head ts)
  , speed = 1
  , button = Nothing
  , buttonChanged = False
  , lastBlockRefresh = 0
  , randomTypes = tail ts
  }
  
-- | General step World function.  Master function that calls other game state modifying functions
stepWorld :: Float -> HGame -> HGame
stepWorld seconds old = new'
  where m = activeMino $ activeBoard old
        action = button old
        allActions = L.map ($ action) [moveMinoToBottomAndStop, moveAction, rotateAction, hintUpdate]
        new = L.foldl (flip (.)) id allActions $ old
        new' = stepMoveDown seconds new
                  
stepMoveDown :: Float -> HGame -> HGame
stepMoveDown seconds old = new' { lastBlockRefresh = newTime }
  where b = activeBoard old
        m = activeMino b
        startTime = lastBlockRefresh old
        shouldMove = (startTime + seconds) > (speed old)
        newTime = if shouldMove
                  then 0
                  else startTime + seconds
        couldMove = couldMinoMoveDown m (activeBoard old)
        new = if (shouldMove && (not couldMove))
                 then stopMino $ old
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

moveDown :: HGame -> HGame
moveDown g = moveAction (Just ADown) g

moveMinoToBottomAndStop :: Maybe Action -> HGame -> HGame
moveMinoToBottomAndStop action old =
     if action == Just AUp && (buttonChanged old)
        then stopMino $ old { activeBoard = b { activeMino = newM }, buttonChanged = False }
        else old
  where b = activeBoard old
        m = activeMino b
        delta = findMinoBottom b m
        newM = moveMino m delta
        
stopMino :: HGame -> HGame
stopMino old = old { activeBoard = b { activeMino = newActiveMino, settledBlocks = newSettled, hintBlocks = newHint }
                   , randomTypes = tail $ randomTypes old
                   }
  where b = activeBoard old
        mino = activeMino b
        newType = head $ randomTypes old
        newSettled = insertBlocksToSettled b (minoBlocks mino)
        newActiveMino = makeMino newType
        newHintBlocks = makeHintBlocks newActiveMino
        newHint = insertBlocksToHint b newHintBlocks
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module HGame where

import Debug.Trace
import Board
import Block
import Mino
import WSClient
import Timer
import Data.List as L
import Data.Map as M
import Graphics.Gloss.Data.Color
import Data.Aeson
import qualified Data.Text as T (pack, unpack, Text)
import Control.Concurrent
import GHC.Generics

data State = NotStarted | Started | MultiStarted | Over | ConnectionError
  deriving (Show, Eq, Generic)

instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON State

data HGame = Game
  { activeBoard :: Board
  , opponentBoard :: Maybe Board
  , nextMino :: Mino
  , level :: Level
  , lineScore :: Int
  , holdMino :: Maybe Mino
  , minoMovedYet :: Bool
  , button :: Maybe Action
  , timers :: Map String Timer
  , buttonChanged :: Bool
  , score :: Int
  , randomTypes :: [MinoType]
  , state :: State
  }
  
instance Timeable HGame where
  getTimers h = timers h
  setTimers h ts = h { timers = ts }
  getTimerCallbacks = M.fromList $ [ ("moveDownTimer",stepMoveDown)
                                   , ("directionalTimer",id)
                                   , ("lockTimer",id)
                                   , ("animationTimer",animateLines)
                                   , ("clearTimer",clearLines)
                                   ]

instance Show HGame where
  show h = show ( [show.activeBoard,show.nextMino,show.level,show.button,
            show.buttonChanged, show.timers, show.score, show.state] <*> [h] )

instance ToJSON HGame where
  toJSON sta@(Game ab _ nm l ls hm mmy b _ bc sc _ st) =
    object [ "activeBoard" .= (ab)
           , "nextMino" .= (nm)
           , "level" .= l
           , "lineScore" .= ls
           , "holdMino" .= hm
           , "minoMovedYet" .= mmy
           , "button" .= b
           , "buttonChanged" .= bc
           , "score" .= sc
           , "state" .= st
           ]
    

instance FromJSON HGame where
  parseJSON = withObject "HGame" objectToHGame
    where objectToHGame v = Game
                              <$> v .: "activeBoard"
                              <*> v .:? "opponentBoard" .!= Nothing
                              <*> v .: "nextMino"
                              <*> v .: "level"
                              <*> v .: "lineScore"
                              <*> v .: "holdMino"
                              <*> v .: "minoMovedYet"
                              <*> v .: "button"
                              <*> v .:? "timers" .!= M.empty
                              <*> v .: "buttonChanged"
                              <*> v .: "score"
                              <*> v .:? "randomTypes" .!= []
                              <*> v .: "state"
  
-- | Initialize the game with this game state.
initialState :: [MinoType] -> HGame
initialState ts = is { activeBoard = betterb }
  where is = Game
              { activeBoard = startingBoard $ (head $ tail ts)
              , opponentBoard = Nothing
              , nextMino = makeMino $ head ts
              , minoMovedYet = False
              , holdMino = Nothing
              , state = NotStarted
              , level = 1
              , lineScore = 0
              , button = Nothing
              , timers = defaultTimers
              , buttonChanged = False
              , score = 0
              , randomTypes = tail $ tail ts
              }
        b = activeBoard is
        betterb = newBoardWithActiveMino (activeMino b) b    


  
-- Line Score Constants
lineScoreCalc :: Level -> Int -> Int
lineScoreCalc l s | s == 0 = 0 
              | s == 1 = 40 * l
              | s == 2 = 100 * l
              | s == 3 = 300 * l
              | s == 4 = 1200 * l
              | otherwise = error "Invalid"
  
-- | General step World function.  Master function that calls other game state modifying functions
stepWorld :: Float -> HGame -> HGame
stepWorld seconds old = if (state n3) == Over then noChangeGameOver else n3
  where action = button old
        --newClearTimer = updateClearTimer seconds (clearTimer $ activeBoard old)
        --n1 = addClearLineTimersIfNeeded old
        n2 = updateTimers old seconds
        --newBoard = (activeBoard old) { clearTimer = newClearTimer }
        -- n2 = old { timers = newTimers
        --         , activeBoard = newBoard
        --         }
        allActions = L.map ($ action) [moveMinoToBottomAndStop, moveAction, rotateAction, holdAction, hintUpdate]
        n3 = L.foldl (flip (.)) id allActions $ n2
        --n4 = stepMoveDown n3
        --n5 = clearLinesState n3
        noChangeGameOver = old { state = Over }

stepMoveDown :: HGame -> HGame
stepMoveDown old = new
  where b = activeBoard old
        m = activeMino b
        didTheMinoMove = minoMovedYet old
        couldMove = couldMinoMoveDown m (activeBoard old)
        new = if (not couldMove)
                 then if didTheMinoMove
                          then (stopMino old)
                          else (stopMino old { state = Over })
                 else moveDown old
                  
moveAction :: Maybe Action -> HGame -> HGame
moveAction action old = 
    if (action == Nothing || (not cMovedBlocks))
       then old
       else old { activeBoard = b { activeMino = newM } }
  where b = activeBoard old
        m = activeMino b
        shouldMoveLeftOrRight = isCycled $ (timers old) ! "directionalTimer"
        (deltaX, deltaY) = case (action, shouldMoveLeftOrRight) of
                              (Just ALeft, True) -> (1,0)
                              (Just ARight, True) -> (-1,0)
                              (Just ADown, _) -> (0,1)
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
        loc = minoLocation m
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
   if hintBlocks b == M.empty && lbr == 0
      then old { activeBoard = b { hintBlocks = ( M.fromList $ (L.map (\x-> ((coordinate x), x)) (makeHintBlocks m))) } }
      else if action == Nothing && not (lbr == 0)
      then old
      else old { activeBoard = b { hintBlocks = newHintBlocks } }
  where b = activeBoard old
        m = activeMino b
        (Timer _ lbr _ _) = (timers old) ! "moveDownTimer"
        (_,minoBottom) = findMinoBottom b m
        hB = sort $ getAllHintBlocks b
        mC = sort $ (coordinate) <$> (minoBlocks m)
        newHintBlocks = insertBlocksToHint (zipWith (\(mX,mY) hh -> moveBlockAbsolute (mX, mY + minoBottom) hh) mC hB)
        
addClearLineTimersIfNeeded :: HGame -> HGame
addClearLineTimersIfNeeded old = if doesClearTimerExist old
                                    then old
                                    else if shouldClear
                                            then old { timers = newTimers }
                                            else old
  where b = activeBoard old
        clearableBlocks = L.concat $ findCompleteLines b
        shouldClear = (length clearableBlocks) > 0
        clearTimerExists = not $ (M.lookup "clearTimer" (timers old)) == Nothing
        newTimers = M.insert "clearTimer" createClearTimer $ M.insert "animationTimer" createClearAnimationTimer (timers old)
        
animateLines :: HGame -> HGame
animateLines old = old { activeBoard = b { settledBlocks = newAnimatedSettledBlocks } }
  where b = activeBoard old
        clearableBlocks = L.concat $ findCompleteLines b
        brightenedBlocks = L.map (\x -> x { blockColor = light $ blockColor x }) clearableBlocks
        newAnimatedSettledBlocks = L.foldr (\x acc -> M.insert (coordinate x) x acc) (settledBlocks b) brightenedBlocks
        
clearLines :: HGame -> HGame
clearLines old = new { activeBoard = newB
                     , timers = newTimersUpdatedSpeed
                     , score = newScore
                     , level = newLevel
                     , lineScore = newLines
                     }
  where b = activeBoard old
        clearableBlocks = L.concat $ findCompleteLines b
        shouldClear = (length clearableBlocks) > 0
        newB = if shouldClear
                  then clearCompLines b
                  else b
        newTimers = if shouldClear
                       then M.delete "animationTimer" $ M.delete "clearTimer" (timers old)
                       else timers old
        newScore = if shouldClear
                      then (lineScoreCalc (level old) ((length clearableBlocks) `quot` 10)) + score old
                      else score old
        newLines = if shouldClear
                      then (lineScore old) + ((length clearableBlocks) `quot` 10)
                      else lineScore old
        newLevel = (newLines `div` 10) + 1
        newSpeed = calcSpeed newLevel
        newMoveDownTimer = updateSpeed ((timers old) ! "moveDownTimer") newSpeed
        newTimersUpdatedSpeed = M.insert "moveDownTimer" newMoveDownTimer newTimers
        new = stopMinoAfterClearCheck old
        
moveDown :: HGame -> HGame
moveDown g = new
  where new = if doesClearTimerExist g
                 then g
                 else moveAction (Just ADown) (g { minoMovedYet = True})

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
stopMino old = newer
  where b = activeBoard old
        mino = activeMino b
        nMino = nextMino old
        newBoard = newBoardWithActiveMino nMino b
        newSettled = insertBlocksToSettled b (minoBlocks mino)
        settledNew = old { activeBoard = newBoard { settledBlocks = newSettled }}
        new = (addClearLineTimersIfNeeded settledNew) 
        newer = if doesClearTimerExist new
                   then new
                   else stopMinoAfterClearCheck new     
        
stopMinoAfterClearCheck :: HGame -> HGame
stopMinoAfterClearCheck old = old { randomTypes = tail $ randomTypes old
                                  , nextMino = newNextMino
                                  , minoMovedYet = False
                                  }
               
  where b = activeBoard old
        mino = activeMino b
        newType = head $ randomTypes old
        newNextMino = makeMino newType

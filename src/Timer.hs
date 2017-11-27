{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Timer where

import Debug.Trace
import Data.Aeson
import Data.Maybe
import Data.Map as M
import Data.List as L
import Control.Monad.Cont
import GHC.Generics

-- | The Timeable type class is a potentially reusable timer callback handler.
-- | Instances of this class must specify a Map of Timers and associated Map of 
-- | callback functions to execute.  Timers must have the same key as the callback
-- | functions to execute.  Timers can be added or removed at will, however there
-- | must be an associated callback function for each Timer.
-- |
-- | Animation Timers run until a certain time duration has elapsed, while 
-- | Countdown timers run once the time has been completed.  They will repeat on
-- | each cycle until they are removed.
-- |
-- | All Timeable handling is triggered from the updateTimers function.
class Timeable a where
  getTimers :: a -> Map String Timer
  setTimers :: a -> Map String Timer -> a
  -- | Callback functions to modify a when the timer completes a cycle
  getTimerCallbacks :: Map String (a -> a)
  
  -- | Get a callback for a specific Timer.  This function will throw an error if the callback does not exist
  getCallback :: String -> Timer -> (a -> a)
  getCallback k v = case (isCycled v, typeOfTimer v) of
                       (True, Animation)  -> id
                       (True, Countdown)  -> callback
                       (False, Animation) -> callback
                       (False, Countdown) -> id
    where callback = fromJust $ M.lookup k getTimerCallbacks
  
  -- | Cannot update the generic value a so must return modified timers as well
  updateTimers :: a -> Float -> a
  updateTimers a s = newerA
    where m = getTimers a
          updatedTimers = M.map (flip updateTimer $ s) m
          newA = setTimers a updatedTimers
          callbacksToExec = M.elems $ M.mapWithKey getCallback updatedTimers
          newerA = L.foldr (\x acc -> acc . x) (id) callbacksToExec $ newA
          

data TimerType = Countdown | Animation
  deriving (Eq, Show, Generic)
  
instance ToJSON TimerType where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON TimerType
  
data Timer = Timer TimerType Float Float Bool
  deriving (Eq, Show, Generic)
  
instance ToJSON Timer where
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Timer
  
type Level = Int

defaultTimers :: Map String Timer
defaultTimers = M.fromList [ ("moveDownTimer", createNewTimer (Left 1) Countdown)
                           , ("directionalTimer", createNewTimer (Right 0.08) Countdown)
                           , ("lockTimer", createNewTimer (Right 0.5) Countdown)
                        --   , ("clearTimer", createNewTimer (Right 0.30) Countdown)
                        --   , ("animationTimer", createNewTimer (Right 0.30) Animation)
                           ]

calcSpeed :: Level -> Float
calcSpeed l = ns / (fromIntegral 43) 
  where ns = fromIntegral $ (48 - (l * 5))

createNewTimer :: Either Level Float -> TimerType -> Timer
createNewTimer (Left l) ty = Timer ty 0 (calcSpeed l) False
createNewTimer (Right s) ty = Timer ty 0 s False

createClearAnimationTimer :: Timer
createClearAnimationTimer = createNewTimer (Right 0.30) Animation

createClearTimer :: Timer
createClearTimer = createNewTimer (Right 0.30) Countdown

doesClearTimerExist :: (Timeable a) => a -> Bool
doesClearTimerExist h = not $ (M.lookup "clearTimer" (getTimers h)) == Nothing
  
updateTimer :: Timer -> Float -> Timer
updateTimer (Timer Countdown startTime speed _) seconds = updatedTimer
  where shouldMove = (startTime + seconds) > speed
        updatedTimer = if shouldMove
                  then Timer Countdown 0 speed True
                  else Timer Countdown (startTime + seconds) speed False
updateTimer (Timer Animation startTime speed _) seconds = updatedTimer
  where shouldMove = (startTime + seconds) <= speed
        updatedTimer = if shouldMove
                  then Timer Animation (startTime + seconds) speed False
                  else Timer Animation 0 speed True

updateSpeed :: Timer -> Float -> Timer
updateSpeed (Timer t a s b) ns = Timer t a ns b
                  
isCycled :: Timer -> Bool
isCycled (Timer _ _ _ ic) = ic

speed :: Timer -> Float
speed (Timer _ _ s _) = s

typeOfTimer :: Timer -> TimerType
typeOfTimer (Timer ty _ _ _) = ty
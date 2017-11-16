{-# LANGUAGE OverloadedStrings #-}
module Timer where

import Data.Aeson
import Data.Map as M

data Timer = Timer Float Float Bool
  deriving (Eq, Show)
  
instance ToJSON Timer where
  toJSON ti@(Timer a b c) =
    object [ "lastRefresh" .= a
           , "speed" .= b
           , "isCycled" .= c
           ]
  
instance FromJSON Timer where
  parseJSON = withObject "Timer" objectToTimer
    where objectToTimer v = Timer
                              <$> v .: "lastRefresh"
                              <*> v .: "speed"
                              <*> v .: "isCycled"
  
type Level = Int

defaultTimers :: Map String Timer
defaultTimers = M.fromList [ ("moveDownTimer", createNewTimer $ Left 1)
                           , ("horizontalTimer", createNewTimer $ Right 0.08)
                           , ("lockTimer", createNewTimer $ Right 0.5)
                           ]

calcSpeed :: Level -> Float
calcSpeed l = ns / (fromIntegral 43) 
  where ns = fromIntegral $ (48 - (l * 5))

createNewTimer :: Either Level Float -> Timer
createNewTimer (Left l) = Timer 0 (calcSpeed l) False
createNewTimer (Right s) = Timer 0 s False
  
updateTimer :: Timer -> Float -> Timer
updateTimer (Timer startTime speed _) seconds = new
  where shouldMove = (startTime + seconds) > speed
        new = if shouldMove
                  then Timer 0 speed True
                  else Timer (startTime + seconds) speed False
                  
updateTimers :: Map String Timer -> Float -> Map String Timer
updateTimers m seconds = M.map (flip updateTimer $ seconds) m

updateSpeed :: Timer -> Float -> Timer
updateSpeed (Timer a s b) ns = Timer a ns b
                  
isCycled :: Timer -> Bool
isCycled (Timer _ _ ic) = ic

speed :: Timer -> Float
speed (Timer _ s _) = s
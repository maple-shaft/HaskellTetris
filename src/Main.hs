{-# LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace
import Network.Socket as S
import Network.WebSockets
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Exception
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Char8 as Char8
import System.IO as SIO
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Block
import Board
import HGame
import Mino
import HConsole
import GlossUtilities
import Menu
import WSClient

-- constants
host :: String
host = "localhost"
                                                               
port :: Int
port = 3000

width, height, offset, fps :: Int
width = 600
height = 800
offset = 10
fps = 15

window :: Display
window = InWindow "HTris" (width, height) (offset, offset)

background :: Color
background = greyN 0.2

render :: Picture -> HGame -> IO Picture
render bg game = return $ pictures $ renderPictures $ state game
  where b = activeBoard game
        scorePane = renderScorePane $ score game
        nextBox = renderNextMino $ nextMino game
        holdBox = renderHoldMino $ holdMino game
        levelBox = renderLevelBox $ level game
        menuBox = renderMenu
        gameOverPicture = scale 0.5 0.5 $ translate (-350) 0 $ text "GAME OVER"
        renderPictures c
            | c == Started || c == MultiStarted  = [bg, (renderBoard b), scorePane, nextBox, holdBox, levelBox]
            | c == NotStarted = [bg, menuBox]
            | otherwise       = [bg, renderRestartButton, gameOverPicture]

addAnOutgoingMessage out state = addMessage out (T.pack $ Char8.unpack $ encode state)

testEvent :: Maybe (MVar [T.Text]) -> Event -> HGame -> IO HGame
testEvent o (EventKey k pressed _ c) old =
    case o of
      Nothing  -> return newSt
      Just out -> addAnOutgoingMessage out newSt >> return newSt
  where newSt = case (k, pressed) of
                   (_, Up)      -> old { button = Nothing }
                   ((SpecialKey KeyLeft), _) -> old { button = Just ALeft }
                   ((SpecialKey KeyRight), _) -> old { button = Just ARight }
                   ((SpecialKey KeyDown), _) -> old { button = Just ADown }
                   ((SpecialKey KeyUp), _) -> old { button = Just AUp, buttonChanged = True }
                   ((Char 'd'), _) -> old { button = Just AD, buttonChanged = True }
                   ((Char 'a'), _) -> old { button = Just AA, buttonChanged = True }
                   ((Char 's'), _) -> old { button = Just AS, buttonChanged = True }
                   ((MouseButton LeftButton), _) -> newState $ state old
                   (_,_) -> old
        randTypes = randomTypes old
        startOverState = initialState randTypes   
        invokeClick = case (checkClickEvent c) of
                                 Just StartButton -> startOverState { state = Started }
                                 Just ExitButton  -> startOverState { state = MultiStarted }
                                 _                -> old
        newState h
            | h == Started = old
            | otherwise    = invokeClick

testEvent _ _ old = return old

step :: Float -> HGame -> IO HGame
step seconds old | (state old) == Started = return steppedWorld
                      | (state old) == MultiStarted = return steppedWorld
                      | otherwise = return old
  where steppedWorld = stepWorld seconds old
        steppedWorldJSON = encode steppedWorld
        
app :: [MinoType] -> Picture -> ClientApp ()
app ts bg conn = do
  SIO.putStrLn "Connected"
  out <- (newMVar [(T.pack "K|12345")]) :: IO (MVar [T.Text])
  inc <- (newMVar []) :: IO (MVar [T.Text])
  let mqs = MessageQueues { incoming = inc, outgoing = out }
  _ <- receiveMessages conn inc
  _ <- sendMessages conn out
  playIO window background fps (initialState ts) (render bg) (testEvent $ Just out) step

       
main :: IO()
main = do
  ts <- getRandomTypes
  bg <- backgroundPic
  result <- try $ S.withSocketsDo $ runClient host port "" (app ts bg) :: IO (Either SomeException ())
  case result of
        Left _ -> SIO.putStrLn "No server found!"
                     >>  playIO window
                                background
                                fps
                                (initialState ts)
                                (render bg)
                                (testEvent Nothing)
                                step
        Right _ -> return () 
   

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `Prelude.elem` [1..8] && r' `Prelude.elem` [1..8])  
    return (c',r')
    
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `Prelude.elem` in3 start
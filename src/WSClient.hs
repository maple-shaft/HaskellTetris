{-# LANGUAGE OverloadedStrings #-}
module WSClient where

import Network.WebSockets
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Debug.Trace

data MessageQueues = MessageQueues { incoming :: MVar [T.Text]
                                   , outgoing :: MVar [T.Text]
                                   }

receiveMessages :: Connection -> MVar [T.Text] -> IO ThreadId
receiveMessages conn incoming = forkIO $ forever $ do
                       msg <- receiveData conn
                       trace (T.unpack msg) $ liftIO $ T.putStrLn msg
                       
sendMessages :: Connection -> MVar [T.Text] -> IO ThreadId
sendMessages conn outgoing = forkIO $ forever $ do
  _ <- threadDelay 10000
  nextToSend <- modifyMVar outgoing getFirstMessage
  if nextToSend == Nothing
     then return ()
     else trace (T.unpack $ fromJust nextToSend) $ sendTextData conn (fromJust nextToSend)
  
getFirstMessage :: [T.Text] -> IO ([T.Text], Maybe T.Text)
getFirstMessage [] = return ([], Nothing)
getFirstMessage list = return ((init list), (Just (last list)))

addMessage :: MVar [T.Text] -> T.Text -> IO ()
addMessage mv message = modifyMVar_ mv (\a -> return $ message : a)
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Web.Twitter.Conduit
import           TwitterUtils
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)
import           Network.Socket      (withSocketsDo)
import           System.Environment  (getEnv)
import           Wuss                (runSecureClient)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

app :: WS.ClientApp ()
app connection = do
    putStrLn "Connected to Infura Websocket!"

    let tweetWsData = do
            wsReceivedData <- WS.receiveData connection
            putStrLn "\n\n--------------------"
            liftIO $ T.putStrLn wsReceivedData
            tweet wsReceivedData

    threadId <- forkIO $ forever $ do tweetWsData

    let putStdinIntoWs = do
            line <- T.getLine
            unless (line == "close") $ WS.sendTextData connection line >> putStdinIntoWs

    let printThreadId = putStrLn ("ThreadId = " ++ show threadId)

    putStdinIntoWs
    printThreadId
    WS.sendClose connection ("Connection closed!" :: Text)
    
tweet :: Text -> IO ()
tweet status = do
      let maxTweetChars = 280
      let limitedStatus = T.take maxTweetChars status
      T.putStrLn $ "\n\nTweet to post: " <> limitedStatus
      twInfo <- getTWInfoFromEnv
      manager <- newManager tlsManagerSettings
      response <- call twInfo manager $ statusesUpdate limitedStatus
      putStrLn ("\n\nTwitter API Response: " ++ show response)

main :: IO ()
main = do
  infuraProjectId <- getEnv "TWEETH_INFURA_PROJECT_ID"
  let wsPort = 443
  let wsInfuraUrl = "mainnet.infura.io"
  let wsInfuraPath = "/ws/v3/" ++ infuraProjectId
  withSocketsDo $ runSecureClient wsInfuraUrl wsPort wsInfuraPath app
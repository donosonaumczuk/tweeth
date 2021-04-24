{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

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

    let putWsIntoStdout = do
            receivedData <- WS.receiveData connection
            liftIO $ T.putStrLn receivedData

    threadId <- forkIO $ forever $ do putWsIntoStdout

    let putStdinIntoWs = do
            line <- T.getLine
            unless (T.length line == 0) $ WS.sendTextData connection line >> putStdinIntoWs

    let printThreadId = putStrLn ("ThreadId = " ++ show threadId)

    putStdinIntoWs
    printThreadId
    WS.sendClose connection ("Connection closed!" :: Text)

main :: IO ()
main = do
  infuraProjectId <- getEnv "TWEETH_INFURA_PROJECT_ID"
  let wsPort = 443
  let wsInfuraUrl = "mainnet.infura.io"
  let wsInfuraPath = "/ws/v3/" ++ infuraProjectId
  withSocketsDo $ runSecureClient wsInfuraUrl wsPort wsInfuraPath app
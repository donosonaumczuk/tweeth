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
    _ <- forkIO $ forever $ do tweetWsData connection
    sendEthSubscribeRequest connection
    loopUnless T.null
    WS.sendClose connection ("Connection closed!" :: Text)

loopUnless :: (Text -> Bool) -> IO ()
loopUnless loopConditionOverText = do
          line <- T.getLine
          unless (loopConditionOverText line) $ loopUnless loopConditionOverText

tweetWsData :: WS.Connection -> IO ()
tweetWsData connection = do
        wsReceivedData <- WS.receiveData connection
        putStrLn "\n\n--------------------"
        liftIO $ T.putStrLn wsReceivedData
        tweet wsReceivedData

sendEthSubscribeRequest :: WS.Connection -> IO ()
sendEthSubscribeRequest connection = do
        let requestBodyText = T.pack "{\"jsonrpc\":\"2.0\", \"id\": 2, \"method\": \"eth_subscribe\",\
            \\"params\": [\"logs\", {\"address\": \"0x6b175474e89094c44da98b954eedeac495271d0f\"}]}"
        WS.sendTextData connection requestBodyText

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

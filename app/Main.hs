{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Monad              (forever)
import           Data.Text                  (Text)
import           Eth.Types
import           Network.Socket             (withSocketsDo)
import           System.Environment         (getEnv)
import           Twitter.Utils.Auth
import           Web.Twitter.Conduit
import           Wuss                       (runSecureClient)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Eth.Events.Dai             as DaiEvents
import qualified Eth.Events.ProofOfHumanity as PohEvents
import qualified Network.WebSockets         as WS

events :: [TweetableEthEvent]
events = [DaiEvents.transfer, PohEvents.addSubmission]

app :: WS.ClientApp ()
app connection = do
    putStrLn "Connected to Infura Websocket!"
    sendEthSubscribeRequestsToInfura connection
    _ <- forever $ do tweetEveryEthEventReceivedFromWs connection
    WS.sendClose connection ("Infura connection finished!" :: Text)

sendEthSubscribeRequestsToInfura :: WS.Connection -> IO ()
sendEthSubscribeRequestsToInfura connection = do
    let daiTransferRequest = T.pack "{\"jsonrpc\":\"2.0\", \"id\": 1, \"method\": \"eth_subscribe\",\
        \\"params\": [\"logs\", {\"address\": \"0x6b175474e89094c44da98b954eedeac495271d0f\",\
        \\"topics\": [\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"]}]}"
    let pohAddSubmissionRequest = T.pack "{\"jsonrpc\":\"2.0\", \"id\": 2, \"method\": \"eth_subscribe\",\
        \\"params\": [\"logs\", {\"address\": \"0xC5E9dDebb09Cd64DfaCab4011A0D5cEDaf7c9BDb\",\
        \\"topics\": [\"0x803727a67d35270dc2c090dc4f9cba1f9818a7200e65c2087eca187851fd6b19\"]}]}"
    WS.sendTextData connection daiTransferRequest
    WS.sendTextData connection pohAddSubmissionRequest
    
tweetEveryEthEventReceivedFromWs :: WS.Connection -> IO ()
tweetEveryEthEventReceivedFromWs connection = do
    putStrLn "\n\n---------------------------------------"
    wsData <- WS.receiveData connection
    putStrLn ("\n\n-- Websocket data:\n" ++ show wsData)
    postTweet . wsDataToTweet $ wsData

wsDataToTweet :: Text -> Maybe Text
wsDataToTweet text = do
    ethSubResponse <- fromJsonText text
    findAndMapEventAsTweet events ethSubResponse

postTweet :: Maybe Text -> IO ()
postTweet (Just tweet) = do
    T.putStrLn $ "\n\n-- Tweet status to post:\n" <> tweet
    twitterCredentials <- getTWInfoFromEnv
    httpsManager <- newManager tlsManagerSettings
    twitterResponse <- call twitterCredentials httpsManager $ statusesUpdate tweet
    putStrLn $ "\n\n-- Twitter API Response:\n" ++ show twitterResponse
postTweet Nothing = putStrLn "\n\n-- Nothing to tweet this time :/"

main :: IO ()
main = do
    infuraProjectId <- getEnv "TWEETH_INFURA_PROJECT_ID"
    let wsPort = 443
    let wsInfuraUrl = "mainnet.infura.io"
    let wsInfuraPath = "/ws/v3/" ++ infuraProjectId
    withSocketsDo $ runSecureClient wsInfuraUrl wsPort wsInfuraPath app

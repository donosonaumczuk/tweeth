{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           Control.Concurrent      (forkIO)
import           Control.Monad           (forever, unless)
import           Control.Monad.Trans     (liftIO)
import           DaiEthEvents
import           Data.Aeson              (decode, FromJSON)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8Builder)
import           EthTypes
import           Network.Socket          (withSocketsDo)
import           PohEthEvents
import           System.Environment      (getEnv)
import           TwitterUtils
import           Web.Twitter.Conduit
import           Wuss                    (runSecureClient)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Network.WebSockets       as WS

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
    putStrLn "\n\n---------------------------------------"
    liftIO $ putStrLn ("\n\n-- Websocket event:\n" ++ show wsReceivedData)
    maybeTweetResultData (responseToStatus wsReceivedData)

sendEthSubscribeRequest :: WS.Connection -> IO ()
sendEthSubscribeRequest connection = do
    let daiTransferRequest = T.pack "{\"jsonrpc\":\"2.0\", \"id\": 1, \"method\": \"eth_subscribe\",\
        \\"params\": [\"logs\", {\"address\": \"0x6b175474e89094c44da98b954eedeac495271d0f\",\
        \\"topics\": [\"0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\"]}]}"
    let pohSubmissionRequest = T.pack "{\"jsonrpc\":\"2.0\", \"id\": 2, \"method\": \"eth_subscribe\",\
        \\"params\": [\"logs\", {\"address\": \"0xC5E9dDebb09Cd64DfaCab4011A0D5cEDaf7c9BDb\",\
        \\"topics\": [\"0x803727a67d35270dc2c090dc4f9cba1f9818a7200e65c2087eca187851fd6b19\"]}]}"
    WS.sendTextData connection daiTransferRequest
    WS.sendTextData connection pohSubmissionRequest

decode'' :: FromJSON a => Text -> Maybe a
decode'' = decode . toLazyByteString . encodeUtf8Builder

responseToStatus :: Text -> Maybe Text
responseToStatus text = do
    ethSubResponse <- decode'' text :: Maybe EthSubscription
    if matchesSubscription DaiTransfer ethSubResponse then return $ toTweet DaiTransfer ethSubResponse
    else if matchesSubscription PohSubmission ethSubResponse then return $ toTweet PohSubmission ethSubResponse
    else Nothing

maybeTweetResultData :: Maybe Text -> IO ()
maybeTweetResultData (Just status) = do
    logStatusIfPresent status
    twInfo <- getTWInfoFromEnv
    manager <- newManager tlsManagerSettings
    response <- call twInfo manager $ statusesUpdate status
    putStrLn $ "\n\n-- Twitter API Response:\n" ++ show response
maybeTweetResultData Nothing = putStrLn "\n\n-- Nothing to tweet this time :)"

logStatusIfPresent :: Text -> IO ()
logStatusIfPresent status = T.putStrLn $ "\n\n-- Tweet status to post:\n" <> status

main :: IO ()
main = do
    infuraProjectId <- getEnv "TWEETH_INFURA_PROJECT_ID"
    let wsPort = 443
    let wsInfuraUrl = "mainnet.infura.io"
    let wsInfuraPath = "/ws/v3/" ++ infuraProjectId
    withSocketsDo $ runSecureClient wsInfuraUrl wsPort wsInfuraPath app

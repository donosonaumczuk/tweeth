{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           EthTypes
import           Web.Twitter.Conduit
import           TwitterUtils
import           Control.Concurrent      (forkIO)
import           Control.Monad           (forever, unless)
import           Control.Monad.Trans     (liftIO)
import           Data.Text               (Text)
import           Network.Socket          (withSocketsDo)
import           System.Environment      (getEnv)
import           Wuss                    (runSecureClient)
import           Data.Aeson              (decode, FromJSON)
import           Data.Text.Encoding      (encodeUtf8Builder)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.Char               (isAsciiUpper, isAsciiLower)
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
    putStrLn "\n\n--------------------"
    liftIO $ T.putStrLn wsReceivedData
    maybeTweetResultData (responseToStatus wsReceivedData)

sendEthSubscribeRequest :: WS.Connection -> IO ()
sendEthSubscribeRequest connection = do
    let requestBodyText = T.pack "{\"jsonrpc\":\"2.0\", \"id\": 2, \"method\": \"eth_subscribe\",\
        \\"params\": [\"logs\", {\"address\": \"0x6b175474e89094c44da98b954eedeac495271d0f\"}]}"
    WS.sendTextData connection requestBodyText

decode'' :: FromJSON a => Text -> Maybe a
decode'' = decode . toLazyByteString . encodeUtf8Builder

responseToStatus :: Text -> Maybe Text
responseToStatus text = do
    ethSubResponse <- decode'' text :: Maybe EthSubscription
    getStatusFromEthSub ethSubResponse

--TODO: This can be written without the cases? Using better the do-notation
maybeTweetResultData :: Maybe Text -> IO ()
maybeTweetResultData (Just status) = do
    logStatusIfPresent status
    twInfo <- getTWInfoFromEnv
    manager <- newManager tlsManagerSettings
    response <- call twInfo manager $ statusesUpdate status
    putStrLn ("\n\nTwitter API Response: " ++ show response)
maybeTweetResultData Nothing = putStrLn "\n\nNothing in maybeTweetResultData"

getStatusFromEthSub :: EthSubscription -> Maybe Text
getStatusFromEthSub (EthSubscription _ _ (EthParams _ (EthResult _ _ _ txHash _ _ _ txData txTopics))) =
    return (T.pack ("\
        \[New DAI Transfer]\n\
        \- FROM: " ++ (T.unpack . formatTextTopicAsEthAddress) (txTopics!!1) ++ "\n\
        \- TO: " ++ (T.unpack . formatTextTopicAsEthAddress) (txTopics!!2) ++ "\n\
        \- AMOUNT: " ++ formatAmount (show (hexStringToInteger (txDataWithout0x txData))) ++ " DAI\n\
        \etherscan.io/tx/" ++ T.unpack txHash
    ))

formatTextTopicAsEthAddress :: Text -> Text
formatTextTopicAsEthAddress text = T.append (T.pack "0x") (T.drop 26 text)

logStatusIfPresent :: Text -> IO ()
logStatusIfPresent status = T.putStrLn $ "\n\nTweet to post: " <> status

--TODO: Take in account les than 1 DAI values!
formatAmount :: String -> String
formatAmount str = (\l s -> take (l - 18) s ++ "," ++ drop (l - 18) s) (length str) str

txDataWithout0x :: Text -> String
txDataWithout0x txData = drop 2 (T.unpack txData)

hexStringToInteger :: String -> Integer
hexStringToInteger [] = 0
hexStringToInteger str = fromIntegral z + 16 * hexStringToInteger (init str)
    where z = let y = last str
            in if isAsciiUpper y
                then fromEnum y - 55
            else if isAsciiLower y
                then fromEnum y - 87
            else fromEnum y - 48

main :: IO ()
main = do
    infuraProjectId <- getEnv "TWEETH_INFURA_PROJECT_ID"
    let wsPort = 443
    let wsInfuraUrl = "mainnet.infura.io"
    let wsInfuraPath = "/ws/v3/" ++ infuraProjectId
    withSocketsDo $ runSecureClient wsInfuraUrl wsPort wsInfuraPath app

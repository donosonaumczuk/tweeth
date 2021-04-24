--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import           Wuss                (runSecureClient)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app connection = do
    putStrLn "Connected!"

    let putWsIntoStdout = do
            receivedData <- WS.receiveData connection
            liftIO $ T.putStrLn receivedData

    -- Fork a thread that writes WS data to stdout
    threadId <- forkIO $ forever $ do putWsIntoStdout

    -- Read from stdin and write to WS
    let putStdinIntoWs = do
            line <- T.getLine
            unless (T.length line == 0) $ WS.sendTextData connection line >> putStdinIntoWs

    let printThreadId = putStrLn ("ThreadId = " ++ show threadId)

    putStdinIntoWs
    printThreadId
    WS.sendClose connection ("Bye!" :: Text)


--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" app
--main = withSocketsDo $ runSecureClient "mainnet.infura.io" 443 "/ws/v3/YOUR-PROJECT-ID" app
-- {"jsonrpc":"2.0", "id": 2, "method": "eth_subscribe", "params": ["logs", {"address": "0x6b175474e89094c44da98b954eedeac495271d0f"}]}

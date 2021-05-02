{-# LANGUAGE OverloadedStrings #-}

module Eth.Events.Dai where

import           Data.Text      (Text)
import           Eth.Types
import qualified Data.Text      as T
import qualified EthFormatUtils as F

daiDecimals :: Int
daiDecimals = 18

transfer :: TweetableEthEvent
transfer = TweetableEthEvent {
    matches = matchesTransferEvent,
    asTweet = transferEventAsTweet
}

matchesTransferEvent :: EthSubscription -> Bool
matchesTransferEvent (EthSubscription _ _ (EthParams _ (EthResult _ _ _ _ _ _ contractAddress _ txTopics))) =
    contractAddress == "0x6B175474E89094C44Da98b954EedeAC495271d0F"
        && head txTopics == "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"

transferEventAsTweet :: EthSubscription -> Text
transferEventAsTweet (EthSubscription _ _ (EthParams _ (EthResult _ _ _ txHash _ _ _ txData txTopics))) =
    T.pack ("[New DAI Transfer]\n\
        \- FROM: " ++ F.formatTextTopicAsEthAddressStr (txTopics!!1) ++ "\n\
        \- TO: " ++ F.formatTextTopicAsEthAddressStr (txTopics!!2) ++ "\n\
        \- AMOUNT: " ++ F.formatTxDataAsAmount daiDecimals txData ++ " DAI\n\
        \etherscan.io/tx/" ++ T.unpack txHash
    )

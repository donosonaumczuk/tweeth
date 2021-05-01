{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module EthTypes where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data EthSubscription = EthSubscription {
    jsonrpc :: Text,
    method  :: Text,
    params  :: EthParams
} deriving (Show, Generic, ToJSON, FromJSON)

data EthParams = EthParams {
    subscription :: Text,
    result       :: EthResult
} deriving (Show, Generic, ToJSON, FromJSON)

data EthResult = EthResult {
    removed          :: Bool,
    logIndex         :: Text,
    transactionIndex :: Text,
    transactionHash  :: Text,
    blockHash        :: Text,
    blockNumber      :: Text,
    address          :: Text,
    resultData       :: Text,
    topics           :: [Text]
} deriving (Show)

instance ToJSON EthResult where
    toJSON EthResult {
          removed = removed,
          logIndex = logIndex,
          transactionIndex = transactionIndex,
          transactionHash = transactionHash,
          blockHash = blockHash,
          blockNumber = blockNumber,
          address = address,
          resultData = resultData,
          topics = topics
    } = object [
          "removed" .= removed,
          "logIndex"  .= logIndex,
          "transactionIndex" .= transactionIndex,
          "transactionHash" .= transactionHash,
          "blockHash" .= blockHash,
          "blockNumber" .= blockNumber,
          "address" .= address,
          "data" .= resultData,
          "topics" .= topics
        ]

instance FromJSON EthResult where
    parseJSON = withObject "EthResult" $ \obj -> do
        removed <- obj .: "removed"
        logIndex <- obj .: "logIndex"
        transactionIndex <- obj .: "transactionIndex"
        transactionHash <- obj .: "transactionHash"
        blockHash <- obj .: "blockHash"
        blockNumber <- obj .: "blockNumber"
        address <- obj .: "address"
        resultData <- obj .: "data"
        topics <- obj .: "topics"
        return (EthResult {
            removed = removed,
            logIndex = logIndex,
            transactionIndex = transactionIndex,
            transactionHash = transactionHash,
            blockHash = blockHash,
            blockNumber = blockNumber,
            address = address,
            resultData = resultData,
            topics = topics
        })

class EthEvent a where
  matchesSubscription :: a -> EthSubscription -> Bool
  
class TweetableEvent a where
  toTweet :: a -> EthSubscription -> Text

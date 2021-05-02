{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Eth.Types where

import Data.Aeson
import Data.ByteString.Builder (toLazyByteString)
import Data.Text               (Text)
import Data.Text.Encoding      (encodeUtf8Builder)
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
          removed             = removed,
          logIndex            = logIndex,
          transactionIndex    = transactionIndex,
          transactionHash     = transactionHash,
          blockHash           = blockHash,
          blockNumber         = blockNumber,
          address             = address,
          resultData          = resultData,
          topics              = topics
    } = object [
          "removed"          .= removed,
          "logIndex"         .= logIndex,
          "transactionIndex" .= transactionIndex,
          "transactionHash"  .= transactionHash,
          "blockHash"        .= blockHash,
          "blockNumber"      .= blockNumber,
          "address"          .= address,
          "data"             .= resultData,
          "topics"           .= topics
        ]

instance FromJSON EthResult where
    parseJSON = withObject "EthResult" $ \obj -> do
        removed          <- obj .: "removed"
        logIndex         <- obj .: "logIndex"
        transactionIndex <- obj .: "transactionIndex"
        transactionHash  <- obj .: "transactionHash"
        blockHash        <- obj .: "blockHash"
        blockNumber      <- obj .: "blockNumber"
        address          <- obj .: "address"
        resultData       <- obj .: "data"
        topics           <- obj .: "topics"
        return (EthResult {
            removed              = removed,
            logIndex             = logIndex,
            transactionIndex     = transactionIndex,
            transactionHash      = transactionHash,
            blockHash            = blockHash,
            blockNumber          = blockNumber,
            address              = address,
            resultData           = resultData,
            topics               = topics
        })

data TweetableEthEvent = TweetableEthEvent {
    matches :: EthSubscription -> Bool,
    asTweet :: EthSubscription -> Text
}

fromJsonText :: Text -> Maybe EthSubscription
fromJsonText = decode . toLazyByteString . encodeUtf8Builder

findEventAndMapAsTw :: [TweetableEthEvent] -> EthSubscription -> Maybe Text
findEventAndMapAsTw = findAndMapEvent asTweet

findAndMapEvent :: (TweetableEthEvent -> EthSubscription -> a) -> [TweetableEthEvent] -> EthSubscription -> Maybe a
findAndMapEvent _ [] _ = Nothing
findAndMapEvent f (x:xs) sub = if matches x sub 
                                  then return $ f x sub
                               else findAndMapEvent f xs sub

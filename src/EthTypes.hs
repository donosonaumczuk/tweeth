{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module EthTypes where

import           Data.Aeson
import           GHC.Generics
import           Data.Text (Text)
import           Data.Char (isAsciiUpper, isAsciiLower)
import qualified Data.Text as T

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

------

class EthEvent a where
  matchesSubscription :: a -> EthSubscription -> Bool
  toTweet :: a -> EthSubscription -> Text

------

data DaiTransfer = DaiTransfer

instance EthEvent DaiTransfer where
  matchesSubscription _ (EthSubscription _ _ (EthParams _ (EthResult _ _ _ _ _ _ contractAddress _ txTopics))) =
    contractAddress == "0x6B175474E89094C44Da98b954EedeAC495271d0F" 
      && head txTopics == "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef"
  
  toTweet _ (EthSubscription _ _ (EthParams _ (EthResult _ _ _ txHash _ _ _ txData txTopics))) =
    T.pack ("\
        \[New DAI Transfer]\n\
        \- FROM: " ++ (T.unpack . formatTextTopicAsEthAddress) (txTopics!!1) ++ "\n\
        \- TO: " ++ (T.unpack . formatTextTopicAsEthAddress) (txTopics!!2) ++ "\n\
        \- AMOUNT: " ++ formatAmount tokenDecimals (show (hexStringToInteger (txDataWithout0x txData))) ++ " DAI\n\
        \etherscan.io/tx/" ++ T.unpack txHash
    )
    
------

data PohRegister = PohRegister

instance EthEvent PohRegister where
  matchesSubscription _ (EthSubscription _ _ (EthParams _ (EthResult _ _ _ _ _ _ contractAddress _ txTopics))) =
    contractAddress == "0xC5E9dDebb09Cd64DfaCab4011A0D5cEDaf7c9BDb" 
      && head txTopics == "0x803727a67d35270dc2c090dc4f9cba1f9818a7200e65c2087eca187851fd6b19"
  
  toTweet _ (EthSubscription _ _ (EthParams _ (EthResult _ _ _ txHash _ _ _ _ txTopics))) =
    T.pack ("\
        \[New PoH Submission]\n\
        \- ADDRESS: " ++ (T.unpack . formatTextTopicAsEthAddress) (txTopics!!1) ++ "\n\
        \- TX HASH: " ++ T.unpack txHash ++ "\n\
        \Check profile submission here:\n\
        \app.proofofhumanity.id/profile/" ++ (T.unpack . formatTextTopicAsEthAddress) (txTopics!!1)
    )
    
------

formatTextTopicAsEthAddress :: Text -> Text
formatTextTopicAsEthAddress text = T.append (T.pack "0x") (T.drop 26 text)

tokenDecimals :: Int
tokenDecimals = 18

formatAmount :: Int -> String -> String
formatAmount dec str = (\dec'' str'' -> format dec'' str'' (length str'')) dec (appendZerosToReachDecimals dec str)
    where format dec'' str'' strLen'' = take (strLen'' - dec'') str'' ++ "." ++ drop (strLen'' - dec'') str''

appendZerosToReachDecimals :: Int -> String -> String
appendZerosToReachDecimals dec str =
    if length str > dec then str
    else appendZerosToReachDecimals dec ("0" ++ str)

txDataWithout0x :: Text -> String
txDataWithout0x txData = drop 2 (T.unpack txData)

hexStringToInteger :: String -> Integer
hexStringToInteger [] = 0
hexStringToInteger str = fromIntegral z + 16 * hexStringToInteger (init str)
    where z = let y = last str in
            if isAsciiUpper y then fromEnum y - 55
            else if isAsciiLower y then fromEnum y - 87
            else fromEnum y - 48

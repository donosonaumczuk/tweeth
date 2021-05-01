{-# LANGUAGE OverloadedStrings #-}

module PohEthEvents where
  
import           EthTypes
import qualified Data.Text as T
import qualified EthFormatUtils as F

data PohSubmission = PohSubmission

instance EthEvent PohSubmission where
  matchesSubscription _ (EthSubscription _ _ (EthParams _ (EthResult _ _ _ _ _ _ contractAddress _ txTopics))) =
    contractAddress == "0xC5E9dDebb09Cd64DfaCab4011A0D5cEDaf7c9BDb"
      && head txTopics == "0x803727a67d35270dc2c090dc4f9cba1f9818a7200e65c2087eca187851fd6b19"

instance TweetableEvent PohSubmission where
  toTweet _ (EthSubscription _ _ (EthParams _ (EthResult _ _ _ txHash _ _ _ _ txTopics))) =
    T.pack ("\
        \[New PoH Submission]\n\
        \- ADDRESS: " ++ F.formatTextTopicAsEthAddressStr (txTopics!!1) ++ "\n\
        \- TX HASH: " ++ T.unpack txHash ++ "\n\
        \Check profile submission here:\n\
        \app.proofofhumanity.id/profile/" ++ F.formatTextTopicAsEthAddressStr (txTopics!!1)
    )

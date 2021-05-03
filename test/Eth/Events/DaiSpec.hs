{-# LANGUAGE OverloadedStrings #-}

module Eth.Events.DaiSpec (main, spec) where

import Test.Hspec
import Eth.Types
import Data.Text (Text, pack)
import qualified Eth.Events.Dai as DaiEvents

main :: IO ()
main = hspec spec

daiTransferSubscription :: EthSubscription
daiTransferSubscription = EthSubscription "2.0" "eth_subscription" (EthParams "0x1feed340995274b5a3f497fb491dcb4e" 
    (EthResult False "0x5d" "0x27" "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553" 
    "0x5d8fd3ad85a97b941f3e1f50bb54075d1b18644824a50d39039ebd78b87598fb" "0xbbe061" 
    "0x6B175474E89094C44Da98b954EedeAC495271d0F" "0x00000000000000000000000000000000000000000000005426c02c9b218f5071" 
    ["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
     "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f",
      "0x0000000000000000000000002ad95483ac838e2884563ad278e933fba96bc242"]))

daiTransfer :: TweetableEthEvent
daiTransfer = DaiEvents.transfer

readFileAsText :: FilePath -> IO Text
readFileAsText path = do
  x <- readFile path
  return $ pack x

spec :: Spec
spec = do
  describe "Eth.Events.Dai tests" $ do
    it "returns that matches the event" $ do
      daiTransferSubscription `shouldSatisfy` matches daiTransfer
    it "returns the expected tweet text" $ do
      expectedDaiTransferTweet <- readFileAsText "test/Files/daiTransfer.txt"
      asTweet daiTransfer daiTransferSubscription `shouldBe` expectedDaiTransferTweet

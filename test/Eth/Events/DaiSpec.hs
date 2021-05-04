{-# LANGUAGE OverloadedStrings #-}

module Eth.Events.DaiSpec (main, spec) where

import           Eth.Types
import           Test.Hspec
import qualified Data.Text.IO as T
import qualified Eth.Events.Dai as DaiEvents

daiTransferSubscription :: EthSubscription
daiTransferSubscription = EthSubscription "2.0" "eth_subscription" (EthParams "0x1feed340995274b5a3f497fb491dcb4e"
    (EthResult False "0x5d" "0x27" "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
    "0x5d8fd3ad85a97b941f3e1f50bb54075d1b18644824a50d39039ebd78b87598fb" "0xbbe061"
    "0x6B175474E89094C44Da98b954EedeAC495271d0F" "0x00000000000000000000000000000000000000000000005426c02c9b218f5071"
    ["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
     "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f",
      "0x0000000000000000000000002ad95483ac838e2884563ad278e933fba96bc242"]))

subscriptionWithoutTransferTopic :: EthSubscription
subscriptionWithoutTransferTopic = EthSubscription "2.0" "eth_subscription"
    (EthParams "0x1feed340995274b5a3f497fb491dcb4e"
    (EthResult False "0x5d" "0x27" "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
    "0x5d8fd3ad85a97b941f3e1f50bb54075d1b18644824a50d39039ebd78b87598fb" "0xbbe061"
    "0x6B175474E89094C44Da98b954EedeAC495271d0F" "0x00000000000000000000000000000000000000000000005426c02c9b218f5071"
    ["0x2ecd88a2c0ed09d136f175f9843f78c8532de065001dc135d58a9c30a71e933f",
     "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f",
      "0x0000000000000000000000002ad95483ac838e2884563ad278e933fba96bc242"]))

subscriptionWithoutDaiAddress :: EthSubscription
subscriptionWithoutDaiAddress = EthSubscription "2.0" "eth_subscription"
    (EthParams "0x1feed340995274b5a3f497fb491dcb4e"
    (EthResult False "0x5d" "0x27" "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
    "0x5d8fd3ad85a97b941f3e1f50bb54075d1b18644824a50d39039ebd78b87598fb" "0xbbe061"
    "0x12345474E89094C44Da98b954EedeAC49527FFFF" "0x00000000000000000000000000000000000000000000005426c02c9b218f5071"
    ["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
     "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f",
      "0x0000000000000000000000002ad95483ac838e2884563ad278e933fba96bc242"]))

spec :: Spec
spec = do
  describe "Eth.Events.Dai tests" $ do
    it "given dai transfer subscription when calling matches on transfer event then return true" $ do
      daiTransferSubscription `shouldSatisfy` matches DaiEvents.transfer
    it "given dai subscription without transfer topic when calling matches on transfer event then return false" $ do
      subscriptionWithoutTransferTopic `shouldNotSatisfy` matches DaiEvents.transfer
    it "given subscription without dai contract address when calling matches on transfer event then return false" $ do
      subscriptionWithoutDaiAddress `shouldNotSatisfy` matches DaiEvents.transfer
    it "given valid dai transfer subscription when calling asTweet on transfer event then return the expected text" $ do
      expectedDaiTransferTweet <- T.readFile "test/Files/Eth/Events/Dai/expectedTransferTweet.txt"
      asTweet DaiEvents.transfer daiTransferSubscription `shouldBe` expectedDaiTransferTweet

main :: IO ()
main = hspec spec

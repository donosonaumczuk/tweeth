{-# LANGUAGE OverloadedStrings #-}

module Eth.TypesSpec (main, spec) where

import           Data.Aeson     (encode)
import           Eth.Types
import           Test.Hspec
import qualified Data.Text.IO as T

ethSubscription :: EthSubscription
ethSubscription = EthSubscription "2.0" "eth_subscription" (EthParams "0x1feed340995274b5a3f497fb491dcb4e"
    (EthResult False "0x5d" "0x27" "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
    "0x5d8fd3ad85a97b941f3e1f50bb54075d1b18644824a50d39039ebd78b87598fb" "0xbbe061"
    "0x6B175474E89094C44Da98b954EedeAC495271d0F" "0x00000000000000000000000000000000000000000000005426c02c9b218f5071"
    ["0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
     "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f",
      "0x0000000000000000000000002ad95483ac838e2884563ad278e933fba96bc242"]))

event :: TweetableEthEvent
event = TweetableEthEvent {
    matches = (== "0x6B175474E89094C44Da98b954EedeAC495271d0F") . ((address . result) . params),
    asTweet = (transactionHash . result) . params
}

alwaysMatchingEvent :: TweetableEthEvent
alwaysMatchingEvent = TweetableEthEvent {
    matches = const True,
    asTweet = const "Constant tweet!"
}

neverMatchingEvent :: TweetableEthEvent
neverMatchingEvent = TweetableEthEvent {
    matches = const False,
    asTweet = const "I don't match with anything!"
}

spec :: Spec
spec = do
  describe "Eth.Types tests" $ do
    it "given subscription as json when calling fromJsonText then return just the expected EthSubscription" $ do
      jsonEthSubscription <- T.readFile "test/Files/Eth/Types/ethSubscription.json"
      fromJsonText jsonEthSubscription `shouldBe` Just ethSubscription
    it "given subscription when calling show then match the expected show output" $ do
      showEthSubscription <- readFile "test/Files/Eth/Types/ethSubscription.txt"
      show ethSubscription `shouldBe` showEthSubscription
    it "given subscription when calling encode then return just the expected json" $ do
      expectedEncodedJson <- readFile "test/Files/Eth/Types/encodedEthSubscription.json"
      show (encode ethSubscription) `shouldBe` show expectedEncodedJson
    it "given subscription that follows event matching condition when calling matches then return true" $ do
      ethSubscription `shouldSatisfy` matches event
    it "given subscription when calling asTweet implemented as txHash then return the expected one" $ do
      asTweet event ethSubscription `shouldBe` "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
    it "given sub and list of matching events when calling findAndMapEventAsTweet then return just first asTweet" $ do
      findAndMapEventAsTweet [alwaysMatchingEvent, event] ethSubscription `shouldBe` Just "Constant tweet!"
    it "given sub and list of matching events when calling findAndMapEventAsTweet then return just first asTweet" $ do
      let txHash = "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
      findAndMapEventAsTweet [neverMatchingEvent, event] ethSubscription `shouldBe` Just txHash
    it "given sub and list of non matching events when calling findAndMapEventAsTweet then return nothing" $ do
      findAndMapEventAsTweet [neverMatchingEvent] ethSubscription `shouldBe` Nothing
    it "given sub and empty list of events when calling findAndMapEventAsTweet then return nothing" $ do
      findAndMapEventAsTweet [] ethSubscription `shouldBe` Nothing
    it "given subscription when calling every field then must return the expected values for each one" $ do
      jsonrpc ethSubscription `shouldBe` "2.0"
      method ethSubscription `shouldBe` "eth_subscription"
      (subscription . params) ethSubscription `shouldBe` "0x1feed340995274b5a3f497fb491dcb4e"
      (removed . (result . params)) ethSubscription `shouldBe` False
      (logIndex . (result . params)) ethSubscription `shouldBe` "0x5d"
      (transactionIndex . (result . params)) ethSubscription `shouldBe` "0x27"
      (transactionHash . (result . params)) ethSubscription `shouldBe`
        "0x2ecd88a2c0ed09d136f175f9843f78c8532de065001be03ed18bf9e4d550d553"
      (blockHash . (result . params)) ethSubscription `shouldBe`
        "0x5d8fd3ad85a97b941f3e1f50bb54075d1b18644824a50d39039ebd78b87598fb"
      (blockNumber . (result . params)) ethSubscription `shouldBe` "0xbbe061"
      (address . (result . params)) ethSubscription `shouldBe` "0x6B175474E89094C44Da98b954EedeAC495271d0F"
      (resultData . (result . params)) ethSubscription `shouldBe`
        "0x00000000000000000000000000000000000000000000005426c02c9b218f5071"
      (topics . (result . params)) ethSubscription `shouldBe`
        [ "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
          "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f",
          "0x0000000000000000000000002ad95483ac838e2884563ad278e933fba96bc242" ]

main :: IO ()
main = hspec spec

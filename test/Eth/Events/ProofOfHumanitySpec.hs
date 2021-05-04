{-# LANGUAGE OverloadedStrings #-}

module Eth.Events.ProofOfHumanitySpec (main, spec) where

import           Eth.Types
import           Test.Hspec
import qualified Data.Text.IO as T
import qualified Eth.Events.ProofOfHumanity as PohEvents

pohAddSubmission :: EthSubscription
pohAddSubmission = EthSubscription "2.0" "eth_subscription" (EthParams "0x1feed340b3559156f7e39013b6a1edd"
    (EthResult False "0x134" "0x77" "0xc1014379a142881a1859f0faa7bd5ea5c9396b65f19c8746c055f0b7c3f693e7"
    "0xd2eb6976b405c2c25f0f849858e5f89629a29f0816d73dfd391d5c891a10468b" "0xbca937"
    "0xC5E9dDebb09Cd64DfaCab4011A0D5cEDaf7c9BDb" "0x0000000000000000000000000000000000000000000000000000000000000000"
    ["0x803727a67d35270dc2c090dc4f9cba1f9818a7200e65c2087eca187851fd6b19",
     "0x000000000000000000000000bbeb5e0adc0818ac8677b2f78387a8a2e22e8d16"]))

subscriptionWithoutAddSubmissionTopic :: EthSubscription
subscriptionWithoutAddSubmissionTopic = EthSubscription "2.0" "eth_subscription"
    (EthParams "0x1feed340b3559156f7e39013b6a1edd"
    (EthResult False "0x134" "0x77" "0xc1014379a142881a1859f0faa7bd5ea5c9396b65f19c8746c055f0b7c3f693e7"
    "0xd2eb6976b405c2c25f0f849858e5f89629a29f0816d73dfd391d5c891a10468b" "0xbca937"
    "0xC5E9dDebb09Cd64DfaCab4011A0D5cEDaf7c9BDb" "0x0000000000000000000000000000000000000000000000000000000000000000"
    ["0x2ecd88a2c0ed09d136f175f9843f78c8532de065001dc135d58a9c30a71e933f",
      "0x000000000000000000000000bbeb5e0adc0818ac8677b2f78387a8a2e22e8d16"]))

subscriptionWithoutPohAddress :: EthSubscription
subscriptionWithoutPohAddress = EthSubscription "2.0" "eth_subscription" (EthParams "0x1feed340b3559156f7e39013b6a1edd"
    (EthResult False "0x134" "0x77" "0xc1014379a142881a1859f0faa7bd5ea5c9396b65f19c8746c055f0b7c3f693e7"
    "0xd2eb6976b405c2c25f0f849858e5f89629a29f0816d73dfd391d5c891a10468b" "0xbca937"
    "0x6B175474E89094C44Da98b954EedeAC495271d0F" "0x0000000000000000000000000000000000000000000000000000000000000000"
    ["0x803727a67d35270dc2c090dc4f9cba1f9818a7200e65c2087eca187851fd6b19",
     "0x000000000000000000000000bbeb5e0adc0818ac8677b2f78387a8a2e22e8d16"]))

spec :: Spec
spec = do
  describe "Eth.Events.ProofOfHumanity tests" $ do
    it "given poh addSubmission subscription when calling matches on addSubmission then return true" $ do
      pohAddSubmission `shouldSatisfy` matches PohEvents.addSubmission
    it "given poh subscription without addSubmission topic when calling matches on addSubmission then return false" $ do
      subscriptionWithoutAddSubmissionTopic `shouldNotSatisfy` matches PohEvents.addSubmission
    it "given subscription without poh contract address when calling matches on addSubmission then return false" $ do
      subscriptionWithoutPohAddress `shouldNotSatisfy` matches PohEvents.addSubmission
    it "given valid poh addSubmission subscription when calling asTweet on addSubmission then return expected text" $ do
      expectedTweet <- T.readFile "test/Files/Eth/Events/ProofOfHumanity/expectedAddSubmissionTweet.txt"
      asTweet PohEvents.addSubmission pohAddSubmission `shouldBe` expectedTweet

main :: IO ()
main = hspec spec

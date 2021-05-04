{-# LANGUAGE OverloadedStrings #-}

module Eth.Utils.FormatSpec (main, spec) where

import Test.Hspec
import Eth.Utils.Format
import Data.Text        (Text)

spec :: Spec
spec = do
  describe "Eth.Utils.Format tests" $ do
    it "given unformatted topic address when calling formatTextTopicAsEthAddress then return the formatted one" $ do
      let unformattedTopicAddress = "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f" :: Text
      let formattedTopicAddress = "0x7515be43d16f871588adc135d58a9c30a71eb34f" :: Text
      formatTextTopicAsEthAddress unformattedTopicAddress `shouldBe` formattedTopicAddress
    it "given unformatted topic address when calling formatTextTopicAsEthAddressStr then return the formatted one" $ do
      let unformattedTopicAddress = "0x0000000000000000000000007515be43d16f871588adc135d58a9c30a71eb34f" :: Text
      let formattedTopicAddress = "0x7515be43d16f871588adc135d58a9c30a71eb34f" :: String
      formatTextTopicAsEthAddressStr unformattedTopicAddress `shouldBe` formattedTopicAddress
    it "given decimals and txData when calling formatTxDataAsAmount then return the formatted amount" $ do
      let txData = "0x00000000000000000000000000000000000000000000000000000000FA56EA00"
      formatTxDataAsAmount 18 txData `shouldBe` "0.000000004200000000"
      formatTxDataAsAmount 8 txData `shouldBe` "42.00000000"
      formatTxDataAsAmount 9 txData `shouldBe` "4.200000000"
    
main :: IO ()
main = hspec spec

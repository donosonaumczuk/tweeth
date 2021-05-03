module Eth.Events.DaiSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] == (23 :: Int)

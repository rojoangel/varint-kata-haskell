module Varint.Test where
import Varint (toVarint, fromVarint)
import Test.Hspec

main = hspec $ do
  describe "toVarint" $ do
    it "should work on the toVarint examples" $ do
      toVarint 300 `shouldBe` "1010 1100 0000 0010"
  describe "fromVarint" $ do
    it "should work on the fromVarint examples" $ do
      fromVarint "1010 1100 0000 0010" `shouldBe` 300
  describe "round test" $ do
    it "toVarint and fromVarint should work together" $ do
      fromVarint(toVarint 300) `shouldBe` 300
      map (fromVarint . toVarint) [1..300000] `shouldBe` [1..300000]

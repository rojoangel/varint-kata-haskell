module Varint.Test where
import Varint (varint)
import Test.Hspec

main = hspec $ do
  describe "varint" $ do
    it "should work on the examples" $ do
      varint 300 `shouldBe` "1010 1100 0000 0010"

import Test.Hspec
import Control.Exception (evaluate)
import Backup
import Data.Time.Calendar

main :: IO ()
main = hspec $ do
  describe "Backup.fromSnapshot" $ do
    it "should parse a proper snapshot name" $ do
      fromSnapshot "zroot/dataset@Weekly-20190102" `shouldBe`
        (Backup Weekly $ fromGregorian 2019 01 02)

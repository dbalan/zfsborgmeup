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

  describe "Backup.toRun" $ do
    it "empty should not fail" $ do
      let today = fromGregorian 2019 01 01
      (length $ toRun [] today) `shouldBe` (3 :: Int)
    it "a single daily backup should produce monthly and weekly" $ do
      let today = fromGregorian 2018 1 1
      (toRun [(Backup Daily today)] today) `shouldBe` []

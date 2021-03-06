import Test.Hspec
import Control.Exception (evaluate)
import Backup
import Config
import Data.Time.Calendar

main :: IO ()
main = hspec $ do
  describe "Backup.fromSnapshot" $ do
    it "should parse a proper snapshot name" $ do
      fromSnapshot "zroot/dataset@Weekly-2019-01-02" `shouldBe`
        (Backup Weekly $ fromGregorian 2019 01 02)

  describe "Backup.toRun" $ do
    it "empty should not fail" $ do
      let today = fromGregorian 2019 1 1
      (length $ toRun today []) `shouldBe` (3 :: Int)
    it "a single daily backup should produce monthly and weekly" $ do
      let today = fromGregorian 2019 1 1
      (map freq $ toRun today [Backup Daily (fromGregorian 2018 1 1)]) `shouldBe`
        [Daily, Weekly, Monthly]
    it "a monthly and weekly snapshot exists, run just daily" $ do
     let today = fromGregorian 2019 2 12
         yesterday = fromGregorian 2019 2 11
         sunday = fromGregorian 2019 2 9
         start = fromGregorian 2019 2 1
     (map freq $ toRun today
       [Backup Daily yesterday, Backup Weekly sunday, Backup Monthly start]) `shouldBe`
       [Daily]
    it "everything exists, don't run anything" $ do
      let today = fromGregorian 2019 2 12
          yesterday = fromGregorian 2019 2 11
          sunday = fromGregorian 2019 2 9
          start = fromGregorian 2019 2 1
      (map freq $ toRun today
        [Backup Daily yesterday, Backup Daily today, Backup Weekly sunday, Backup Monthly start])
        `shouldBe` []
  describe "Backup.toPrune" $ do
    it "empty list should give you empty" $ do
      let conf = BackupConfig { dataset = "zroot/usr"
                              , monthly = 3
                              , weekly = 2
                              , daily = 4 }
      toPrune conf [] `shouldBe` []
    it "single frequency less than limit" $ do
      let conf = BackupConfig { dataset = "zroot/usr"
                              , monthly = 3
                              , weekly = 2
                              , daily = 2 }
          today = fromGregorian 2019 2 12
          yesterday = fromGregorian 2019 2 11
          blist = [Backup Daily today, Backup Daily yesterday]
      toPrune conf blist `shouldBe` []
    it "prune a larger list, return oldest" $ do
      let conf = BackupConfig { dataset = "zroot/usr"
                              , monthly = 3
                              , weekly = 2
                              , daily = 2 }
          today = fromGregorian 2019 2 12
          yesterday = fromGregorian 2019 2 11
          start = fromGregorian 2019 2 1
          blist = [Backup Daily today, Backup Daily start,
                    Backup Daily yesterday]
      toPrune conf blist `shouldBe` [Backup Daily start]
    it "mixed test" $ do
      let conf = BackupConfig { dataset = "zroot/usr"
                              , monthly = 3
                              , weekly = 2
                              , daily = 2 }
          today = fromGregorian 2019 2 12
          yesterday = fromGregorian 2019 2 11
          sunday = fromGregorian 2019 2 9
          start = fromGregorian 2019 2 1
          lastYear = fromGregorian 2018 1 1
          yearBefore = fromGregorian 2017 1 1
          blist = [ Backup Daily today
                  , Backup Daily start
                  , Backup Daily yesterday
                  , Backup Weekly today
                  , Backup Weekly lastYear
                  , Backup Weekly yearBefore
                  ]
      toPrune conf blist `shouldBe` [ Backup Daily start
                                    , Backup Weekly yearBefore]

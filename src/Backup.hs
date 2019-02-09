module Backup ( Backup(..)
              , Frequency
              , toRun
              ) where

import Data.Ord
import Data.List
import qualified Data.Time.Calendar as T

data Frequency = Daily | Weekly | Monthly
  deriving (Show, Eq)

data Backup = Backup { freq :: Frequency
                     , date :: T.Day
                     }
              deriving (Show, Eq)

latestBackup :: [Backup] -> Frequency -> Backup
latestBackup lb frq = head sortedBkup
  where
    bfreq = filter (\b-> (freq b) == frq) lb
    sortedBkup = sortBy (\b1 b2 -> compare (date b1) (date b2)) bfreq

backupFreq :: Frequency -> Integer
backupFreq f = case f of
  Daily -> 1
  Weekly -> 7
  Monthly -> 30

nextBackup :: Backup -> Backup
nextBackup bk = Backup f nd
  where
    f = freq bk
    nd = T.addDays (backupFreq f) $ date bk

singleToRun :: [Backup] -> Frequency -> Backup
singleToRun bk f = nextBackup $ latestBackup bk f

backup0 = map (\f -> Backup f $ T.fromGregorian 0 0 0) [Monthly, Daily, Weekly]
-- toRun looks at all the current backups and figures out next backups to run
toRun :: [Backup] -> [Backup]
toRun bk = map (singleToRun $ bk++backup0) [Daily, Weekly, Monthly]

-- should we run the backup today
shouldRun :: Backup -> T.Day -> Bool
shouldRun bk d = d <= date bk

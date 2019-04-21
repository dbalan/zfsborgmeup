
module Backup ( Backup(..)
              , Frequency(..)
              , toRun
              , fromSnapshot
              ) where

import Data.Ord
import Data.List
import qualified Data.Time.Calendar as T
import Data.List.Split

data Frequency = Daily | Weekly | Monthly
  deriving (Show, Eq, Read)

data Backup = Backup { freq :: Frequency
                     , date :: T.Day
                     }
              deriving (Eq)

instance Show Backup where
  show bk = (show $ freq bk) ++ "-" ++ (show $ date bk)

instance Ord Backup where
  compare bk1 bk2 = compare (date bk1) (date bk2)

-- FIXME: this ugly parsing
fromSnapshot :: String -> Backup
fromSnapshot str = Backup (read f :: Frequency) (read dt :: T.Day)
    where
      snp = concat $ tail $ splitOn "@" str
      f = head $ splitOn "-" snp
      dt = snd $ splitAt ((length f)+1) snp

latestBackup :: [Backup] -> Frequency -> Backup
latestBackup lb frq = head sortedBkup
  where
    bfreq = filter (\b-> (freq b) == frq) lb
    sortedBkup = reverse $ sortBy (\b1 b2 -> compare (date b1) (date b2)) bfreq

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
toRun :: T.Day -> [Backup] -> [Backup]
toRun today bk = filter (shouldRun today) $ map (singleToRun $ bk++backup0) [Daily, Weekly, Monthly]

-- should we run the backup on a specific day
shouldRun :: T.Day -> Backup -> Bool
shouldRun d bk = d >= date bk


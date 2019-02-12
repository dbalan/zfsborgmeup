
module Backup ( Backup(..)
              , Frequency(..)
              , toRun
              , fromSnapshot
              ) where

import Data.Ord
import Data.List
import qualified Data.Time.Calendar as T
import Data.List.Split
import qualified Data.Text as T

data Frequency = Daily | Weekly | Monthly
  deriving (Show, Eq, Read)

data Backup = Backup { freq :: Frequency
                     , date :: T.Day
                     }
              deriving (Show, Eq)

-- FIXME: fix this ugly parsing
fromSnapshot :: String -> Backup
fromSnapshot s = Backup freq $ readDate (sfd !! 1)
  where
    sname = splitOn "@" s !! 1
    sfd = splitOn "-" sname
    freq = read (head sfd) :: Frequency

-- FIXME: this parsing is bad
readDate s = T.fromGregorian year month day
  where
    year = read (take 4 s) :: Integer
    month = read (take 2 $ drop 4 s) :: Int
    day = read (take 2 $ drop 6 s) :: Int

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

-- should we run the backup today
shouldRun :: T.Day -> Backup -> Bool
shouldRun d bk = d >= date bk

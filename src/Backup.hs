module Backup ( Date(..)
              , Backup(..)
              , Frequency
              , latestBackup
              ) where

import Data.Ord
import Data.List

data Date = Date { day :: Int
                 , month :: Int
                 , year :: Int
                 }
            deriving (Eq, Show)

instance Ord Date where
  compare d1 d2
    | d1 == d2 = EQ
    | (year d1) > (year d2) = GT
    | (month d1) > (month d2) = GT
    | (day d1) > (day d2) = GT
    | otherwise = LT

data Frequency = Daily | Weekly | Monthly
  deriving (Show, Eq)

data Backup = Backup { freq :: Frequency
                     , date :: Date
                     }
              deriving (Show, Eq)


latestBackup :: [Backup] -> Frequency -> Date
latestBackup lb frq = date $ head sortedBkup
  where
    bfreq = filter (\(Backup f _) -> f == frq) lb
    sortedBkup = sortBy (\b1 b2 -> compare (date b1) (date b2)) bfreq

backupFreq :: Frequency -> Int
backupFreq f = case f of
  Daily -> 1
  Weekly -> 7
  Monthly -> 30


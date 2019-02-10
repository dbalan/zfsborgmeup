module Main where

import Backup
import Data.Time.Calendar
import qualified Data.Text as T
import Turtle
import qualified Turtle.Pattern as P
import qualified Control.Foldl as CF

main :: IO ()
main = do
  putStrLn $ show $ toRun []

-- backs up a specific dataset
backupDataset :: String -> IO ()
backupDataset = undefined

allBackups :: String -> IO [Backup]
allBackups ds = do
  sli <- snapShots ds
  return $ map (fromSnapshot . T.unpack) sli

snapShots :: MonadIO m => String -> m [Text]
snapShots ds = do
  let snap = getSnapList ds
  snlines <- fold snap CF.list
  return $ map lineToText snlines

-- getSnapList :: String -> Shell line
getSnapList ds = grep ptrn (inproc "zfs" ["list", "-t", "snapshot", "-H", "-o", "name"] empty)
  where
    ptrn = P.prefix $ P.text $ T.pack (ds ++ "@")

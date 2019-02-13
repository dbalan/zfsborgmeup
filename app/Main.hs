module Main where

import Backup
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import qualified Data.Text as T
import Turtle
import qualified Turtle.Pattern as P
import qualified Control.Foldl as CF

main :: IO ()
main = backupDataset "testpool/documents"

-- backs up a specific dataset
backupDataset :: String -> IO ()
backupDataset ds = do
  echo $ "starting backup: " <> unsafeToLine ds
  -- FIXME: check if dataset exists
  backups <- allBackups ds
  -- we will always be in UTC. don't screw this up
  today <- getCurrentTime
  let rn = toRun (utctDay today) backups
  echo $ "will run " <> (unsafeToLine $ show (map freq rn))

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
getSnapList ds = grep ptrns (inproc "zfs" ["list", "-t", "snapshot", "-H", "-o", "name"] empty)
  where
    ptrn freq = P.prefix $ P.text $ T.pack (ds ++ "@" ++ freq)
    ptrns = ptrn "Monthly" <|> ptrn "Weekly"  <|> ptrn "Daily"

-- if arg contains new lines, this will explode
unsafeToLine :: String -> Line
unsafeToLine s = unsafeTextToLine $ T.pack s

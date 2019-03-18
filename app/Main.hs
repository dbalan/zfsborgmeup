module Main where

import           Data.Time.Clock (getCurrentTime, UTCTime(..))
import           Turtle
import qualified Data.Text as T
import qualified Turtle.Pattern as P
import qualified Control.Foldl as CF
import           Data.Time.Clock

import           Backup
import           Config

main :: IO ()
main = do
  config <- loadConfig
  mapM_ backupDataset $ map dataset config

-- backs up a specific dataset
backupDataset :: String -> IO ()
backupDataset ds = do
  echo $ "info: starting backup: " <> unsafeToLine ds
  -- FIXME: check if dataset exists
  backups <- allBackups ds
  -- we will always be in UTC. don't screw this up
  today <- getCurrentTime
  let rn = toRun (utctDay today) backups
  echo $ "info: will run " <> (unsafeToLine $ show (map freq rn))
  mapM_ (liftIO . runBackup ds) (map freq rn)
  echo $ "info: all good!"

allBackups :: MonadIO m => String -> m [Backup]
allBackups ds = do
  snlines <- fold (getSnapList ds) CF.list
  return $ map (fromSnapshot . T.unpack . lineToText) snlines

getSnapList :: String -> Shell Line
getSnapList ds = grep ptrns (inproc "zfs" ["list", "-t", "snapshot", "-H", "-o", "name"] empty)
  where
    ptrn freq = P.prefix $ P.text $ T.pack (ds ++ "@" ++ freq)
    ptrns = ptrn "Monthly" <|> ptrn "Weekly"  <|> ptrn "Daily"

-- if arg contains new lines, this will explode
unsafeToLine :: String -> Line
unsafeToLine s = unsafeTextToLine $ T.pack s


runBackup :: MonadIO m => String -> Frequency -> m ExitCode
runBackup ds f = do
  today <- liftIO getCurrentTime
  let backup =  T.pack $ ds ++ "@" ++ (show $ Backup f $ utctDay today)
  shell ("echo zfs snapshot -t " <> backup) empty

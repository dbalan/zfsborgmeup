module Main where

import           Options.Applicative
import           RunBackup
import           Config

data ZfsBorgMeCmd = ShowConfig
                  | RunBackup
                  deriving (Eq, Show)

cmdParser :: Parser ZfsBorgMeCmd
cmdParser = subparser (
  command "show-config" (info (pure ShowConfig) (progDesc "print current config"))
  <> command "run-backup" (info (pure RunBackup) (progDesc "run local-backup")))

cmd :: ParserInfo ZfsBorgMeCmd
cmd = info (cmdParser <**> helper)
  (fullDesc
  <> progDesc "Backup manager for local (zfs) and remote (borg) backup"
  <> header "ZFSBorgMeUp - zfs backup manager")

main :: IO ()
main = do
  opts <- execParser cmd
  case opts of
    ShowConfig -> putStrLn "show-config"
    RunBackup -> backup

backup :: IO ()
backup = do
  config <- loadConfig
  mapM_ backupDataset $ map dataset config

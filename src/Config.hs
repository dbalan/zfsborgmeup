{-# LANGUAGE QuasiQuotes #-}

module Config ( BackupConfig(..)
              , loadConfig
              ) where

import           Control.Monad (when)
import           GHC.Generics (Generic)
import qualified Data.ByteString as B
import           Data.Yaml as Y
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Path (reldir, relfile, toFilePath, File, Path, Abs, (</>))
import qualified Path.IO as P

{-| BackupConfig is configuration for a single dataset
    Program Config is of type [BackupConfig]
-}
data BackupConfig = BackupConfig { dataset :: String
                                 , monthly :: Int
                                 , weekly :: Int
                                 , daily :: Int
                                 } deriving (Show, Generic)

instance FromJSON BackupConfig
instance ToJSON BackupConfig

-- Code for the config parsing is inspired from @donatello's haskell code.
configFile :: MonadIO m => m (Path Abs File)
configFile = do
  dir <- P.getXdgDir P.XdgConfig (Just [reldir|zfsborgmeup|])
  P.ensureDir dir
  let cfgfp = dir </> [relfile|config.yaml|]
  ok <- P.doesFileExist cfgfp
  when (not ok) $
    liftIO $ Y.encodeFile (toFilePath cfgfp) ([] :: [BackupConfig])
  return cfgfp

loadConfig :: MonadIO m => m [BackupConfig]
loadConfig = do
  cfp <- configFile
  Y.decodeFileThrow (toFilePath cfp)

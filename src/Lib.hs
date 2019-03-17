module Lib ( module Exports
           , BkError(..)
           ) where

import Control.Exception as Exports (Exception, throwIO)
import Control.Exception (Exception)

data BkError = BkError String
  deriving (Eq, Show)
instance Exception BkError

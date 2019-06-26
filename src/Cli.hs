module Cli where

import qualified Crypto.Hash as Hash
import qualified Crypto.Hash.Algorithms as Hash
import           Data.Text (Text)
import qualified Data.Text as Text


data Rev = Rev Text
  deriving (Show)


data Migration
  = Migration
  { mRev :: Rev
  , mParent :: Maybe Rev
  , mDescription :: Text
  , mUpgradeFile :: FilePath
  , mUpgradeFileHash :: Hash.Digest Hash.SHA256
  , mDowngradeFile :: Maybe FilePath
  } deriving (Show)


main :: IO ()
main = putStrLn "Hello schemactl"

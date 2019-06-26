{-# LANGUAGE OverloadedStrings #-}

module Cli where

import qualified Crypto.Hash as Hash
import qualified Crypto.Hash.Algorithms as Hash
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as Bs


data Rev = Rev Bs.ByteString
  deriving (Show)


data Migration
  = Migration
  { mRev :: Rev
  , mParent :: Maybe Rev
  , mDescription :: Bs.ByteString
  , mUpgradeFile :: FilePath
  , mUpgradeFileHash :: Hash.Digest Hash.SHA256
  , mDowngradeFile :: Maybe FilePath
  } deriving (Show)


data MigrationLine
  = MigrationLine
  { mlRev :: Rev
  , mlParent :: Maybe Rev
  , mlDescription :: Bs.ByteString
  , mlBaseFilePath :: FilePath
  }


indexFileP :: Atto.Parser [MigrationLine]
indexFileP = do
  migrationLines <- Atto.many1 (migrationLineP)
  pure migrationLines


migrationLineP :: Atto.Parser MigrationLine
migrationLineP = do
  revB <- Atto.takeWhile (\c -> c /= '_')
  Atto.char '_'

  description <- Atto.takeWhile (\c -> c /= '.')
  Atto.string ".sql"
  Atto.char '\n'

  pure MigrationLine
    { mlRev = Rev revB
    , mlDescription = description
    , mlParent = Nothing -- Unknown yet
    , mlBaseFilePath = Bs.unpack (revB <> "_" <> description <> ".sql")
    }


main :: IO ()
main = putStrLn "Hello schemactl"

{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import           Control.Exception (Exception, IOException, tryJust)
import           Control.Monad (when)
import           Control.Monad.Except (ExceptT, liftEither, withExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import           Data.Bifunctor (first)
import qualified Data.ByteString as Bs
import           System.IO.Error (isDoesNotExistError, isPermissionError)


data Config = Config
  deriving (Eq, Show)


instance Aeson.FromJSON Config where
  parseJSON = const $ pure Config


data ConfigErr
  = JsonDecodeErr String
  | ReadIOErr IOException
  deriving (Eq, Show)


loadFromFile :: FilePath -> ExceptT ConfigErr IO Config
loadFromFile configFile = do
  contents <- liftIOCatching [isDoesNotExistError, isPermissionError] ReadIOErr (Bs.readFile configFile)
  liftEither $ first JsonDecodeErr $ Aeson.eitherDecodeStrict contents


validate :: Config -> ExceptT [ConfigErr] IO Config
validate = undefined


liftIOCatching :: Exception e => MonadIO m => [e -> Bool] -> (e -> e') -> IO a -> ExceptT e' m a
liftIOCatching guards excTransform action = do
  let
    handler exc =
      if or (fmap (\guard -> guard exc) guards)
        then Just $ excTransform exc
        else Nothing
  liftEither =<< (liftIO $ tryJust handler action)


isReadIOErr :: ConfigErr -> Bool
isReadIOErr (ReadIOErr _) = True
isReadIOErr _ = False

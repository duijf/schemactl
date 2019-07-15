module ConfigSpec where

import Test.Hspec

import           Control.Monad.Except (ExceptT, liftEither, withExceptT, runExceptT)
import qualified Config
import           Data.Either (isLeft, fromLeft)

spec :: Spec
spec = do
  describe "Config" $ do
    it "reports missing files" $ do
      let missingPath = "nosuchdir/schemactl.json"
      configOrErr <- runExceptT $ Config.loadFromFile missingPath
      configOrErr `shouldSatisfy` (\err -> isLeft err && Config.isReadIOErr (fromLeft undefined err))

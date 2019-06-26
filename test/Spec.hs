import qualified Cli as Cli
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "noop test" $ do
    it "runs and succeeds" $ do
      1 `shouldBe` 1

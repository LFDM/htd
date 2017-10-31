import           Test.Tasty

-- import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck
-- import qualified Test.Tasty.SmallCheck as SC
import qualified ReadWriteTest

suites = [ReadWriteTest.suite]

suite :: TestTree
suite = testGroup "htd" suites

main :: IO ()
main = defaultMain suite

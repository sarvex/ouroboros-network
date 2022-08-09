module Main (main) where

import qualified System.Directory as Dir
import           System.IO.Temp
import           Test.Tasty

import qualified Test.Ouroboros.Storage
import           Test.Util.TestMode (defaultMainWithIohkTestMode)

main :: IO ()
main = do
  sysTmpDir <- Dir.getTemporaryDirectory
  withTempDirectory sysTmpDir "cardano-s-m" $ \tmpDir ->
    defaultMainWithIohkTestMode (tests tmpDir)

tests :: FilePath -> TestTree
tests tmpDir =
  testGroup "ouroboros-storage"
  [ Test.Ouroboros.Storage.tests tmpDir
  ]

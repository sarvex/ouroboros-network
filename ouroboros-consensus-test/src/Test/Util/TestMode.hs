{-# LANGUAGE LambdaCase #-}
-- | A @tasty@ command-line option for enabling nightly tests
module Test.Util.TestMode (
    IohkTestMode (..)
  , askIohkTestMode
  , defaultMainWithIohkTestMode
  , iohkTestModeIngredient
  , resetQuickCheckTests
  ) where

import           Data.Proxy (Proxy (..))
import           Options.Applicative (metavar)
import           Test.Tasty
import           Test.Tasty.Ingredients
import           Test.Tasty.Options
import           Test.Tasty.QuickCheck
import           Test.Util.Nightly

-- | 'defaultMain' extended with 'iohkTestEnvIngredient'
defaultMainWithIohkTestMode :: TestTree -> IO ()
defaultMainWithIohkTestMode testTree =
    defaultMainWithIngredients (iohkNightlyIngredient : iohkTestModeIngredient : defaultIngredients) $
    askIohkTestMode (defaultTestEnv $ askOption iohkNightlyToTestMode)

    where
      -- If iohk-nightly is already set, change the mode to nightly
      iohkNightlyToTestMode :: IohkNightlyEnabled -> TestTree
      iohkNightlyToTestMode (IohkNightlyEnabled True) = adjustOption (const Nightly) testTree
      iohkNightlyToTestMode _                         = testTree


-- | This ingredient merely adds the 'IohkTestEnv' 'Option' to the
-- @tasty@ command-line parser.
iohkTestModeIngredient :: Ingredient
iohkTestModeIngredient =
    TestManager [Option (Proxy :: Proxy IohkTestMode)] $
    \_optionSet _testTree -> Nothing

-- | Query and adjust options for `IohkTestMode`
askIohkTestMode :: (IohkTestMode -> TestTree) -> TestTree
askIohkTestMode = askOption

-- | Setup the default test environment
defaultTestEnv :: TestTree -> IohkTestMode -> TestTree
defaultTestEnv testTree = \case
  Nightly -> adjustOption (const (QuickCheckTests 1000000)) testTree
  CI      -> adjustOption (const (QuickCheckTests 10000)) testTree
  Dev     -> testTree

-- | Reset quickcheck tests
resetQuickCheckTests :: (QuickCheckTests -> QuickCheckTests) -> TestTree -> TestTree
resetQuickCheckTests f = adjustOption $ const (f (QuickCheckTests 100))

-- | An 'Option' that indicates the environment in which to run tests.
data IohkTestMode =
  -- | Run more expensive tests
    Nightly
  -- | Run tests for the continuous integration, slightly more expensive
  | CI
  -- | A fast tests for developers to run before committin
  | Dev

safeReadTestMode :: String -> Maybe IohkTestMode
safeReadTestMode "nightly" = Just Nightly
safeReadTestMode "ci"      = Just CI
safeReadTestMode "dev"     = Just Dev
safeReadTestMode _         = Nothing

instance IsOption IohkTestMode where
  defaultValue = Dev
  parseValue = safeReadTestMode
  optionName = pure "iohk-test-mode"
  optionHelp = pure "Enable a test mode (specific to IOHK). \
      \ The 'dev' mode runs default number of quickcheck tests (100), \
      \ 'nightly' mode runs 1_000_000 quickcheck tests, and \
      \ 'ci' mode runs 100_000 quickcheck tests. \
      \ Individual tests can be adjusted to run less/more depending \
      \ upon time taken to run them"

  -- Use typical Un*x syntax for Boolean flags
  optionCLParser = mkOptionCLParser $ metavar "nightly|ci|dev"

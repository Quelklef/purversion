module Main (purverse) where

import Prelude

import Effect (Effect)
import Data.Array as Array
import Data.Natural (Natural, intToNat, natToInt)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (class Traversable, traverse)
import Data.Foldable (sum, foldl)
import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits as String
import Partial.Unsafe (unsafeCrashWith)


--------------------
-- Util functions --

natToString :: Natural -> String
natToString = natToInt >>> show

stringToNat :: String -> Maybe Natural
stringToNat = parseInt >>> map intToNat

-- | Base-10
parseInt :: String -> Maybe Int
parseInt = String.toCharArray >>> traverse digitValue >>> map sum
  where digitValue digit = String.indexOf (Pattern $ String.singleton digit) "0123456789"

-- | splitOnce "&" "a & b & c" == Just $ "a " /\" b & c"
splitOnce :: String -> String -> Maybe (String /\ String)
splitOnce delimiter string = do
  idx <- String.indexOf (Pattern delimiter) string
  before <- string # String.slice 0 idx
  let string' = string <> " "  -- https://github.com/purescript/purescript-strings/issues/143
  after <- string' # String.slice (idx + String.length delimiter) (String.length string)
  pure $ before /\ after


---------------
-- Main code --

foreign import nativeSet :: String -> String -> Effect Unit
foreign import nativeGet :: String -> Effect String

foreign import exists :: String -> Effect Boolean

-- Convert a (version, payload) to a localstorage item
mkItem :: Natural -> String -> String
mkItem version payload = natToString version <> ":" <> payload

-- Convert from a localstorage item to a (verison, payload) pair
unItem :: String -> Maybe { version :: Natural, payload :: String }
unItem string = do
  versionString /\ payload <- splitOnce ":" string
  version <- stringToNat versionString
  pure { version, payload }

unsafeRetrieve :: String -> Effect { version :: Natural, payload :: String }
unsafeRetrieve key = do
  keyExists <- exists key
  if not keyExists then do
    pure { version: intToNat 0, payload: "" }
  else do
    item <- nativeGet key
    case unItem item of
      Just x -> pure x
      Nothing -> unsafeCrashWith $ "[Purversion] Bad internal state! Is something else modifying localStorage? The key " <> show key <> " has the following value, which is invalid: " <> show item

purverse
  :: forall dm mm state
   . Monad mm
  => Traversable mm
  => Monad dm
  => String
  -> Array (String -> mm String)
  -> (state -> String)
  -> (String -> dm state)
  -> Effect (mm { save :: state -> Effect Unit
                , load :: Effect (dm state) })

purverse key migrations encode decode = runMigrations # (map <<< map) (const { load, save })
  where

    load :: Effect (dm state)
    load = do
      payload <- _.payload <$> unsafeRetrieve key
      pure $ decode payload

    save :: state -> Effect Unit
    save state = do
      version <- _.version <$> unsafeRetrieve key
      nativeSet key $ mkItem version (encode state)
      pure unit

    runMigrations :: Effect (mm Unit)
    runMigrations = do
      let targetVersion = intToNat $ Array.length migrations
      { payload: unmigrated, version: unmigratedVersion } <- unsafeRetrieve key
      let neededMigrations = Array.drop (natToInt unmigratedVersion) migrations
      let migrate = foldl (>=>) (pure <<< identity) neededMigrations
      let migratedM = migrate unmigrated
      migratedM # traverse \migrated -> do
        nativeSet key $ mkItem targetVersion migrated
        pure unit

module Database.Purversion (make, load, save, Purverse, Migrated) where

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
      Nothing -> unsafeCrashWith $ "[Purversion] Bad internal val! Is something else modifying localStorage? The key " <> show key <> " has the following value, which is invalid: " <> show item

-- | Represents a localStorage key which is managed by Purversion
type Purverse mv de val =
  { key :: String
  , migrations :: Array (String -> mv String)
  , encode :: val -> String
  , decode :: String -> de val
  }

data Migrated mv de val = Migrated (Purverse mv de val)

make
  :: forall mv de val
   . Monad mv
  => Traversable mv
  => Purverse mv de val
  -> Effect (mv (Migrated mv de val))
make pv = migrate pv # (map <<< map) (const $ Migrated pv)

load :: forall mv de val. Migrated mv de val -> Effect (de val)
load (Migrated pv) = do
  payload <- _.payload <$> unsafeRetrieve pv.key
  pure $ pv.decode payload

save :: forall mv de val. Migrated mv de val -> val -> Effect Unit
save (Migrated pv) val = do
  version <- _.version <$> unsafeRetrieve pv.key
  nativeSet pv.key $ mkItem version (pv.encode val)
  pure unit

migrate
  :: forall mv de val
   . Monad mv
  => Traversable mv
  => Purverse mv de val
  -> Effect (mv Unit)
migrate pv = do
  let targetVersion = intToNat $ Array.length pv.migrations
  { payload: unmigrated, version: unmigratedVersion } <- unsafeRetrieve pv.key
  let neededMigrations = Array.drop (natToInt unmigratedVersion) pv.migrations
  let bigMigration = foldl (>=>) (pure <<< identity) neededMigrations
  let migratedM = bigMigration unmigrated
  migratedM # traverse \migrated -> do
    nativeSet pv.key $ mkItem targetVersion migrated
    pure unit

module Database.Purversion (make, load, save, Purverse) where

import Prelude

import Effect (Effect)
import Data.Array as Array
import Data.Natural (Natural, intToNat, natToInt)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (class Traversable, traverse)
import Data.Foldable (foldl)
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
parseInt = String.toCharArray >>> traverse digitValue >>> map (foldl (\n d -> 10 * n + d) 0)
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

-- Convert a (version, payload) pair to a localstorage item
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

-- | Represents a localStorage key which is managed by Purversion
-- |
-- | By "managed" I mean that Purversion equips the native string localStorage value with:
-- | 1. a rich type, via `encode` and `decode`; and
-- | 2. versioning, via `migrations`
-- |
-- | Each `Purverse` value must be created via `make`. This is to ensure that one cannot
-- | `save` to or `load` from a key that has not been migrated.
-- |
-- | The type variables have the following meaning:
-- |
-- | - `mv :: Type -> Type` provides a context to migrations, allowing failures. Ex: `Either String`, `Maybe`. Also see `make`.
-- |
-- | - `de :: Type -> Type` provides a context to decoding, allowing failures. Ex: `Either String`, `Maybe`. Also see `load`.
-- |
-- | - `val :: Type` is the type of the values being stored by Purversion. Also see `save`, `load`.
data Purverse mv de val = Purverse
  { key :: String
  , migrations :: Array (String -> mv String)
  , encode :: val -> String
  , decode :: String -> de val
  }

-- | Create a Purversion-managed localStorage key.
-- |
-- | This will first run all migrations for the key, failing in `mv` if any migration fails.
-- |
-- | If no failure is encountered, then a `Purversion` value is produced and returned,
-- | which can be used with the rest of the API to interact with the localStorage key.
make
  :: forall mv de val
   . Monad mv
  => Traversable mv
  => { key :: String
     , migrations :: Array (String -> mv String)
     , encode :: val -> String
     , decode :: String -> de val
     }
  -> Effect (mv (Purverse mv de val))
make fields =
  let pv = Purverse fields
  in migrate pv # (map <<< map) (const pv)

-- | Read the value of out a `Purverse` key.
-- |
-- | This reads the `String` value from localStorage, and then
-- | transforms it into a `val` with `.decode`.
-- |
-- | Two kinds of failures are possible:
-- |
-- | 1. In `de`, if the decoding fails.
-- |
-- | 2. In `Effect`, if the localStorage values have been edited by an outside
-- | agent, such as non-Purversion code or the programmer
load :: forall mv de val. Purverse mv de val -> Effect (de val)
load (Purverse pv) = do
  payload <- _.payload <$> unsafeRetrieve pv.key
  pure $ pv.decode payload

-- | Save a value into a `Purverse` key.
-- |
-- | This first encodes the value down to a `String` via `.encode`, and
-- | then saves that string into localStorage.
save :: forall mv de val. Purverse mv de val -> val -> Effect Unit
save (Purverse pv) val = do
  version <- _.version <$> unsafeRetrieve pv.key
  nativeSet pv.key $ mkItem version (pv.encode val)
  pure unit

migrate
  :: forall mv de val
   . Monad mv
  => Traversable mv
  => Purverse mv de val
  -> Effect (mv Unit)
migrate (Purverse pv) = do
  let targetVersion = intToNat $ Array.length pv.migrations
  { payload: unmigrated, version: unmigratedVersion } <- unsafeRetrieve pv.key
  let neededMigrations = Array.drop (natToInt unmigratedVersion) pv.migrations
  let bigMigration = foldl (>=>) (pure <<< identity) neededMigrations
  let migratedM = bigMigration unmigrated
  migratedM # traverse \migrated -> do
    nativeSet pv.key $ mkItem targetVersion migrated
    pure unit

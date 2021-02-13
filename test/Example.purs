module Test.Example where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Foldable (foldl)
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Maybe (Maybe)

import Database.Purversion as Pv


-- Program state type
-- In our case, everyone's favorite: a counter!
type ProgramState = { counter :: Int }

main :: Effect Unit
main = do

  -- Create our managed localStorage key
  pv <- Pv.make

    -- The name of the key
    { key: "program-state"

    -- Data encoding and decoding to/from String
    -- We represent our program as a stringified integer
    , encode: _.counter >>> show
    , decode: \string -> parseInt string <#> \justNumber -> { counter: justNumber }

    -- Data migrations
    -- In our case, we have no migrations, so just give it Identity
    , migrations: [Identity]
    } <#> unwrap  -- remove the Identity

  -- Save a value
  Pv.save pv { counter: 127 }

  -- Load a value!
  got <- Pv.load pv

  log $ show got  -- Just { counter: 6 }


parseInt :: String -> Maybe Int
parseInt = String.toCharArray >>> traverse digitValue >>> map (foldl (\n d -> 10 * n + d) 0)
  where digitValue digit = String.indexOf (Pattern $ String.singleton digit) "0123456789"

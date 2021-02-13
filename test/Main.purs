module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Effect.Exception as Exception
import Control.Monad.Error.Class (throwError)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.String.Utils (includes) as String
import Data.String.Common (toLower) as String
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..))
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)

import Main (purverse)

foreign import establishLocalStorageShim :: Effect Unit

main :: Effect Unit
main = do
  establishLocalStorageShim

  launchAff_ $ runSpec [consoleReporter] do
    describe "purversion" do

      it "subsumes localstorage" do
        { save: saveA, load: loadA } <- liftEffect $ unwrap <$> purverse "subsume-A" noMigrations identity Just
        { save: saveB, load: loadB } <- liftEffect $ unwrap <$> purverse "subsume-B" noMigrations identity Just
        liftEffect (saveA "string a" *> loadA) >>= (_ `shouldEqual` Just "string a")
        liftEffect (saveB "string b" *> loadB) >>= (_ `shouldEqual` Just "string b")
        liftEffect (saveA "gnirts a" *> loadA) >>= (_ `shouldEqual` Just "gnirts a")
        liftEffect (saveB "gnirts b" *> loadB) >>= (_ `shouldEqual` Just "gnirts b")

      it "supports encoding" do
        { save, load } <- liftEffect $ unwrap <$> purverse "encoding" noMigrations String.toLower Just
        liftEffect (save "TeSt" *> load) >>= (_ `shouldEqual` Just "test")

      it "supports failable decoding" do
        -- our "decode" function is to put all in lowercase and crash on any 'x' characters
        let decode s = if String.includes "x" s then Nothing else Just (String.toLower s)
        { save, load } <- liftEffect $ unwrap <$> purverse "decoding" noMigrations identity decode
        liftEffect (save "TeSt" *> load) >>= (_ `shouldEqual` Just "test")
        liftEffect (save "TxSt" *> load) >>= (_ `shouldEqual` Nothing)

      it "supports failable migrations" do

        let migrationsA = [Right <<< (_ <> "howdy"), Right <<< (_ <> " doo"), Right <<< (_ <> "?")]
        { save: saveA, load: loadA } <- liftEffect $ unsafePartial fromRight <$> purverse "migrations" migrationsA identity Just
        liftEffect loadA >>= (_ `shouldEqual` Just "howdy doo?")

        let migrationsB = migrationsA <> [const $ Left "oh no!"]
        expectError do
          migrationResult <- liftEffect $ purverse "migrations" migrationsB identity Just
          case migrationResult of
            Left err -> throwError $ Exception.error err
            Right _ -> pure unit

  where
    noMigrations = [] :: Array (String -> Identity String)

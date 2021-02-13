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

import Database.Purversion as Pv

import Test.Example as Example

foreign import establishLocalStorageShim :: Effect Unit

main :: Effect Unit
main = do
  establishLocalStorageShim

  launchAff_ $ runSpec [consoleReporter] do

    describe "example" do
      it "runs" do
        liftEffect Example.main

    describe "purversion" do

      it "subsumes localstorage" do
        pvA <- liftEffect $ unwrap <$> Pv.make { key: "subsume-A", migrations: noMigrations, encode: identity, decode: Just }
        pvB <- liftEffect $ unwrap <$> Pv.make { key: "subsume-B", migrations: noMigrations, encode: identity, decode: Just }
        liftEffect (Pv.save pvA "string a" *> Pv.load pvA) >>= (_ `shouldEqual` Just "string a")
        liftEffect (Pv.save pvB "string b" *> Pv.load pvB) >>= (_ `shouldEqual` Just "string b")
        liftEffect (Pv.save pvA "gnirts a" *> Pv.load pvA) >>= (_ `shouldEqual` Just "gnirts a")
        liftEffect (Pv.save pvB "gnirts b" *> Pv.load pvB) >>= (_ `shouldEqual` Just "gnirts b")

      it "supports encoding" do
        pv <- liftEffect $ unwrap <$> Pv.make { key: "encoding", migrations: noMigrations, encode: String.toLower, decode: Just }
        liftEffect (Pv.save pv "TeSt" *> Pv.load pv) >>= (_ `shouldEqual` Just "test")

      it "supports failable decoding" do
        -- our "decode" function is to put all in lowercase and crash on any 'x' characters
        let decode s = if String.includes "x" s then Nothing else Just (String.toLower s)
        pv <- liftEffect $ unwrap <$> Pv.make { key: "decoding", migrations: noMigrations, encode: identity, decode: decode }
        liftEffect (Pv.save pv "TeSt" *> Pv.load pv) >>= (_ `shouldEqual` Just "test")
        liftEffect (Pv.save pv "TxSt" *> Pv.load pv) >>= (_ `shouldEqual` Nothing)

      it "supports failable migrations" do

        let migrationsA = [Right <<< (_ <> "howdy"), Right <<< (_ <> " doo"), Right <<< (_ <> "?")]
        pvA <- liftEffect $ unsafePartial fromRight <$> Pv.make { key: "migrations", migrations: migrationsA, encode: identity, decode: Just }
        liftEffect (Pv.load pvA) >>= (_ `shouldEqual` Just "howdy doo?")

        let migrationsB = migrationsA <> [const $ Left "oh no!"]
        expectError do
          migrationResult <- liftEffect $ Pv.make { key: "migrations", migrations: migrationsB, encode: identity, decode: Just }
          case migrationResult of
            Left err -> throwError $ Exception.error err
            Right _ -> pure unit

  where
    noMigrations = [] :: Array (String -> Identity String)

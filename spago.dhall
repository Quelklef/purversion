{ name = "purversion"
, dependencies =
  [ "arrays"
  , "effect"
  , "foldable-traversable"
  , "identity"
  , "naturals"
  , "newtype"
  , "partial"
  , "spec"
  , "strings"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/Quelklef/purversion"
}

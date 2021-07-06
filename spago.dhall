{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quantities"
, dependencies =
  [ "console"
  , "decimals"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "pairs"
  , "prelude"
  , "psci-support"
  , "test-unit"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

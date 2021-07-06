{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quantities"
, dependencies =
  [ "console"
  , "decimals"
  , "effect"
  , "foldable-traversable"
  , "lists"
  , "numbers"
  , "pairs"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

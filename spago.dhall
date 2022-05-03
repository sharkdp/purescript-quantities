{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quantities"
, dependencies =
  [ "decimals"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "numbers"
  , "pairs"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/sharkdp/purescript-quantities"
}

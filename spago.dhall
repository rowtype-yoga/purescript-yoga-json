{ name = "yoga-json"
, license = "MIT"
, repository = "https://github.com/rowtype-yoga/purescript-yoga-json.git"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "partial"
  , "prelude"
  , "record"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

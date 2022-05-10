{ name = "purescript-json"
, license = "MIT"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
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
  , "nonempty"
  , "nullable"
  , "partial"
  , "prelude"
  , "record"
  , "transformers"
  , "typelevel-prelude"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

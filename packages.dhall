let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220610/packages.dhall
        sha256:348212b7c79da7d343bed71b48ed164d426f1977f92196babac49bd560b32e75

in  upstream
  with js-bigints =
    { dependencies = [ "maybe", "prelude" ]
    , version = "v1.2.0"
    , repo = "https://github.com/sigma-andex/purescript-js-bigints"
    }

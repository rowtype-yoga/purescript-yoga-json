let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221116/packages.dhall
        sha256:a0b88cbba414b046d9e7660fb77ed433d238d4c6ab20e75a479c9c6c823f2812

in  upstream
  with 
  js-bigints.version = "v2.0.0"

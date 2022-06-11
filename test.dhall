let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "spec"
              , "spec-discovery"
              , "console"
              , "nonempty"
              , "aff"
              , "strings-extra"
              , "newtype"
              ]
        }

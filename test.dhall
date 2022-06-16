let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "test/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "spec"
              , "spec-discovery"
              , "aff"
              , "strings-extra"
              , "newtype"
              , "debug"
              ]
        }

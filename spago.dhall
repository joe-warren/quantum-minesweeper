{ name = "halogen-project"
, dependencies = 
    [ "console"
    , "effect"
    , "halogen"
    , "halogen-svg-elems"
    , "prelude"
    , "arrays"
    , "foldable-traversable"
    , "integers"
    , "maybe"
    , "tuples"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

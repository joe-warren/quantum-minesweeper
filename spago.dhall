{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "random"
  , "st"
  , "tuples"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

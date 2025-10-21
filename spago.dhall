{ name = "halogen-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-svg-elems"
  , "integers"
  , "maybe"
  , "prelude"
  , "tuples"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

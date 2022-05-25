{ name = "pos6502"
, dependencies =
  [ "effect"
  , "numerics"
  , "prelude"
  , "refs"
  , "transformers"
  , "uint"
  , "word"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

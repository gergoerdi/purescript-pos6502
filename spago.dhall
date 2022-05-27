{ name = "pos6502"
, dependencies =
  [ "arrays"
  , "effect"
  , "integers"
  , "numerics"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
  , "strings"
  , "transformers"
  , "uint"
  , "word"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

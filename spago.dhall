{ name = "score-sheet-builder"
, dependencies = [ 
    "arrays",
    "b64",
    "console", 
    "control",
    "effect", 
    "either",
    "halogen", 
    "integers",
    "js-uri",
    "maybe",
    "ordered-collections",
    "parsing",
    "prelude",
    "strings",
    "tuples",
    "web-html",
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

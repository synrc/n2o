# Used by "mix format"
[
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}"],
  line_length: 80,
  export: [
    locals_without_parens: [some_dsl_call: 2, some_dsl_call: 3]
  ]
]

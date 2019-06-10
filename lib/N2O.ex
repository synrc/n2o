defmodule N2O
  require Record
  import  Record, only: [defrecord: 2, extract: 2]

  defrecord :cx, extract(:cx, from_lib: "n2o/include/n2o.hrl")

end

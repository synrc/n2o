defmodule N2O do
  require Record
  import Record, only: [defrecord: 2, extract: 2]

  defrecord :cx, extract(:cx, from_lib: "n2o/include/n2o.hrl")
  defrecord :pi, extract(:pi, from_lib: "n2o/include/n2o.hrl")
  defrecord :client, extract(:client, from_lib: "n2o/include/n2o.hrl")

  defmacro __using__(opts \\ []) do
    imports =
      opts
      |> Macro.expand(__CALLER__)
      |> Keyword.get(:with, [:n2o])

    Enum.map(imports, fn mod ->
      if Code.ensure_compiled?(mod) do
        upcased = Module.concat([String.upcase(to_string(mod))])

        quote do
          import unquote(upcased)
          alias unquote(mod), as: unquote(upcased)
        end
      else
        IO.warn("🚨 Unknown module #{mod} was requested to be used by :n2o. Skipping.")
      end
    end)
  end
end

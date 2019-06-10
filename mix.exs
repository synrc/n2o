defmodule N2O.Mixfile do
  use Mix.Project
  def deps, do: [ {:ex_doc, ">= 0.0.0", only: :dev}]
  def application, do: [mod: {:n2o, []}]
  def project do
    [ app: :n2o,
      version: "6.6.6",
      description: "N2O DAS MQTT TCP WebSocket",
      package: package(),
      deps: deps()]
  end
  def package do
    [ files: ~w(doc include man priv src test mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/synrc/n2o"}]
  end
end

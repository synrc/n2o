defmodule N2O.Mixfile do
  use Mix.Project
  def deps, do: [ {:ex_doc, ">= 0.0.0", only: :dev},
                 #{:emqtt, github: "emqx/emqtt", tag: "v1.0.0"},
                  {:syn, "~> 1.6.3"} ]
  def application, do: [mod: {:n2o, []}, applications: [:syn]]
  def project do
    [ app: :n2o,
      version: "6.9.0",
      description: "N2O MQTT TCP WebSocket",
      package: package(),
      deps: deps()]
  end
  def package do
    [ files: ~w(doc include man priv lib src test mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/synrc/n2o"}]
  end
end

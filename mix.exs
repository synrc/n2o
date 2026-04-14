defmodule N2O.Mixfile do
  use Mix.Project
  def deps, do: [{:ex_doc, ">= 0.0.0", only: :dev, runtime: false}]
  def application(), do:
    [
      mod: {:n2o, []},
      extra_applications: [:crypto, :xmerl]
    ]

  def project do
    [
      app: :n2o,
      version: "13.4.15",
      description: "N2O MQTT TCP WebSocket",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(include man priv lib src mix.exs LICENSE README.md),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/synrc/n2o"}
    ]
  end
end

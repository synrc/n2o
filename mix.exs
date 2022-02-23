defmodule N2O.Mixfile do
  use Mix.Project
  def deps, do: [{:ex_doc, ">= 0.0.0", only: :dev}]
  def application, do: [mod: {:n2o, []}, applications: [], extra_applications: [:crypto]]

  def project do
    [
      app: :n2o,
      version: "8.12.1",
      description: "N2O MQTT TCP WebSocket",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(doc include man priv lib src test mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/synrc/n2o"}
    ]
  end
end

defmodule N2O.Mixfile do
  use Mix.Project
  def project do
    [app: :n2o,
     version: "6.5.0",
     description: "N2O DAS MQTT TCP WebSocket",
     package: package(),
     deps: deps()]
  end
  defp package do
    [files: ~w(doc include man priv src test mix.exs rebar.config LICENSE),
     licenses: ["ISC"],
     links: %{"GitHub" => "https://github.com/synrc/n2o"}]
  end
  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end

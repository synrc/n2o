defmodule N2O.Mixfile do
  use Mix.Project

  def project do
    [app: :n2o,
     version: "2.9.0",
     description: "N2O Application Server",
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:n2o, []}
    ]
  end

  defp package do
    [files: ~w(c_src doc include priv src LICENSE mix.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/n2o"}]
  end

  defp deps do [
    {:jsone,  github: "sile/jsone"},
    {:cowboy, github: "extend/cowboy"},
    {:gproc,  gitbub: "uwiger/gproc"},
  ]
  end
end

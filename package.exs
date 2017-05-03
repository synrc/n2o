defmodule N2O.Mixfile do
  use Mix.Project

  def project do
    [app: :n2o,
     version: "4.4.0",
     description: "N2O Application Server",
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:n2o, []}]
  end

  defp package do
    [files: ["include", "priv", "samples", "src", "LICENSE", "README.md", "rebar.config"],
     licenses: ["MIT"],
     maintainers: ["Andy Martemyanov", "Namdak Tonpa"],
     name: :n2o,
     links: %{"GitHub" => "https://github.com/synrc/n2o"}]
  end

  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end

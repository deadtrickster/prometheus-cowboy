defmodule Prometheus-cowboy.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus_cowboy
     version: "1.1.0",
     deps: deps(),
     description: description(),
     package: package()]
  end

  defp description do
    """
    prometheus_cowboy
    """
  end

  defp package do
    [maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus_cowboy"},
     files: ["priv", "src", "README.md", "rebar.config"]]
  end

  defp deps do
    []
  end
end

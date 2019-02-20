defmodule PrometheusCowboy.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus_cowboy,
     version: "0.1.7",
     deps: deps(),
     description: description(),
     package: package()]
  end

  defp description do
    """
    Prometheus instrumenters and handlers for Cowboy 1&2.
    """
  end

  defp package do
    [maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus-cowboy"},
     files: ["src", "lib", "README.md", "rebar.config"]]
  end

  defp deps do
    [{:prometheus_httpd, "~> 2.1"}]
  end
end

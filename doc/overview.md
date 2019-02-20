@copyright 2017 Ilya Khaprov <<i.khaprov@gmail.com>>.
@title prometheus_cowboy
@version 0.1.7

@doc

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]

## Exporting metrics with handlers

Cowboy 1:

<pre lang="erlang">
Routes = [
          {'_', [
                 {"/metrics/[:registry]", prometheus_cowboy1_handler, []},
                 {"/", toppage_handler, []}
                ]}
         ]
</pre>

Cowboy 2:

<pre lang="erlang">
Routes = [
          {'_', [
                 {"/metrics/[:registry]", prometheus_cowboy2_handler, []},
                 {"/", toppage_handler, []}
                ]}
         ]
</pre>

## Exporting Cowboy2 metrics

<pre lang="erlang">
  {ok, _} = cowboy:start_clear(http, [{port, 0}],
                               #{env => #{dispatch => Dispatch},
                                 metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
                                 stream_handlers => [cowboy_metrics_h, cowboy_stream_h]})
</pre>

## Contributing

Section order:

- Types
- Macros
- Callbacks
- Public API
- Deprecations
- Private Parts

Install the `git' pre-commit hook:

<pre lang="bash">
./bin/pre-commit.sh install
</pre>

The pre-commit check can be skipped by passing `--no-verify' to `git commit'.

## License

MIT

<!-- Named Links -->

[Hex badge]: https://img.shields.io/hexpm/v/prometheus_cowboy.svg?maxAge=2592000?style=plastic
[Hex link]: https://hex.pm/packages/prometheus_cowboy
[Hex downloads badge]: https://img.shields.io/hexpm/dt/prometheus_cowboy.svg?maxAge=2592000
[Travis badge]: https://travis-ci.org/deadtrickster/prometheus-cowboy.svg?branch=version-3
[Travis link]: https://travis-ci.org/deadtrickster/prometheus-cowboy
[Coveralls badge]: https://coveralls.io/repos/github/deadtrickster/prometheus-cowboy/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/deadtrickster/prometheus-cowboy?branch=master

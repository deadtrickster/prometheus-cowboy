

# prometheus_cowboy #

Copyright (c) 2017 Ilya Khaprov <<i.khaprov@gmail.com>>.

__Version:__ 0.1.7

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]

## Exporting metrics with handlers

Cowboy 1:

```erlang

Routes = [
          {'_', [
                 {"/metrics/[:registry]", prometheus_cowboy1_handler, []},
                 {"/", toppage_handler, []}
                ]}
         ]

```

Cowboy 2:

```erlang

Routes = [
          {'_', [
                 {"/metrics/[:registry]", prometheus_cowboy2_handler, []},
                 {"/", toppage_handler, []}
                ]}
         ]

```

## Exporting Cowboy2 metrics

```erlang

  {ok, _} = cowboy:start_clear(http, [{port, 0}],
                               #{env => #{dispatch => Dispatch},
                                 metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
                                 stream_handlers => [cowboy_metrics_h, cowboy_stream_h]})

```

## Contributing

Section order:

- Types
- Macros
- Callbacks
- Public API
- Deprecations
- Private Parts

Install the `git` pre-commit hook:

```bash

./bin/pre-commit.sh install

```

The pre-commit check can be skipped by passing `--no-verify` to `git commit`.

## License

MIT

[Hex badge]: https://img.shields.io/hexpm/v/prometheus_cowboy.svg?maxAge=2592000?style=plastic
[Hex link]: https://hex.pm/packages/prometheus_cowboy
[Hex downloads badge]: https://img.shields.io/hexpm/dt/prometheus_cowboy.svg?maxAge=2592000
[Travis badge]: https://travis-ci.org/deadtrickster/prometheus-cowboy.svg?branch=version-3
[Travis link]: https://travis-ci.org/deadtrickster/prometheus-cowboy
[Coveralls badge]: https://coveralls.io/repos/github/deadtrickster/prometheus-cowboy/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/deadtrickster/prometheus-cowboy?branch=master


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="prometheus_cowboy.md" class="module">prometheus_cowboy</a></td></tr>
<tr><td><a href="prometheus_cowboy1_handler.md" class="module">prometheus_cowboy1_handler</a></td></tr>
<tr><td><a href="prometheus_cowboy2_handler.md" class="module">prometheus_cowboy2_handler</a></td></tr>
<tr><td><a href="prometheus_cowboy2_instrumenter.md" class="module">prometheus_cowboy2_instrumenter</a></td></tr></table>


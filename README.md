

# prometheus_cowboy #

Copyright (c) 2017 Ilya Khaprov <<i.khaprov@gmail.com>>.

__Version:__ 0.0.2

[![Hex.pm][Hex badge]][Hex link]
[![Hex.pm Downloads][Hex downloads badge]][Hex link]
[![Build Status][Travis badge]][Travis link]
[![Coverage Status][Coveralls badge]][Coveralls link]

## Handlers

Cowboy 1:

```erlang

Routes = [
          {'_', [
                 {"/metrics/[:registry]", prometheus_cowboy1_handler, []},
                 {"/", toppage_handler, []}
                ]}
         ],

```

Cowboy 2:

```erlang

Routes = [
          {'_', [
                 {"/metrics/[:registry]", prometheus_cowboy2_handler, []},
                 {"/", toppage_handler, []}
                ]}
         ],

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
[Travis badge]: https://travis-ci.org/deadtrickster/prometheus_cowboy.svg?branch=version-3
[Travis link]: https://travis-ci.org/deadtrickster/prometheus_cowboy
[Coveralls badge]: https://coveralls.io/repos/github/deadtrickster/prometheus_cowboy/badge.svg?branch=master
[Coveralls link]: https://coveralls.io/github/deadtrickster/prometheus_cowboy?branch=master


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/deadtrickster/prometheus-cowboy/blob/master/doc/prometheus_cowboy.md" class="module">prometheus_cowboy</a></td></tr>
<tr><td><a href="https://github.com/deadtrickster/prometheus-cowboy/blob/master/doc/prometheus_cowboy1_handler.md" class="module">prometheus_cowboy1_handler</a></td></tr>
<tr><td><a href="https://github.com/deadtrickster/prometheus-cowboy/blob/master/doc/prometheus_cowboy2_handler.md" class="module">prometheus_cowboy2_handler</a></td></tr></table>


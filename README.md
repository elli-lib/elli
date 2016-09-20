# elli - Erlang web server for HTTP APIs

[![Travis CI][travis badge]][travis builds]
[![Hex.pm][hex badge]][hex package]
[![Erlang][erlang badge]][erlang downloads]
[![Documentation][doc badge]][docs]
[![MIT License][license badge]](LICENSE)

[travis builds]: https://travis-ci.org/elli-lib/elli
[travis badge]: https://travis-ci.org/elli-lib/elli.svg
[hex badge]: https://img.shields.io/hexpm/v/elli.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/elli
[latest release]: https://github.com/elli-lib/elli/releases/latest
[erlang badge]: https://img.shields.io/badge/erlang-%E2%89%A518.0-red.svg
[erlang downloads]: http://www.erlang.org/downloads
[doc badge]: https://img.shields.io/badge/docs-edown-green.svg
[docs]: doc/README.md
[license badge]: https://img.shields.io/badge/license-MIT-blue.svg

Elli is a webserver you can run inside your Erlang application to
expose an HTTP API. Elli is a aimed exclusively at building
high-throughput, low-latency HTTP APIs. If robustness and performance
is more important than general purpose features, then `elli` might be
for you. If you find yourself digging into the implementation of a
webserver, `elli` might be for you. If you're building web services,
not web sites, then `elli` might be for you.

Elli is used in production at Wooga and Game Analytics. Elli requires
OTP 18.0 or newer.


## Installation

To use `elli` you will need a working installation of Erlang 18.0 (or later).

Add `elli` to your application by adding it as a dependency to your
[`rebar.config`](http://www.rebar3.org/docs/configuration):

```erlang
{deps, [
  %% ...
  {elli, "2.0.0"}
]}.
```

Afterwards you can run:

```sh
$ rebar3 compile
```


## Usage
```sh
$ rebar3 shell
```

```erlang
%% starting elli
1> {ok, Pid} = elli:start_link([{callback, elli_example_callback}, {port, 3000}]).
```


## Further Reading

More information about the features and design philosophy of `elli`, as well as
some adaptable examples can be found in the [overview](doc/).


## License

Elli is licensed under [The MIT License](LICENSE).

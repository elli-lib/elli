# Elli - Erlang web server for HTTP APIs

[![Hex.pm](https://img.shields.io/hexpm/v/elli.svg)](https://hex.pm/packages/elli)
[![Documentation](https://img.shields.io/badge/docs-edown-green.svg)](https://hexdocs.pm/elli/)
[![Erlang](https://img.shields.io/badge/erlang-%E2%89%A520.0-red.svg)](http://www.erlang.org/downloads)
![Common Test](https://github.com/elli-lib/elli/workflows/Common%20Test/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/elli-lib/elli/badge.svg?branch=develop)](https://coveralls.io/github/elli-lib/elli?branch=develop)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Elli is a webserver you can run inside your Erlang application to
expose an HTTP API. It is aimed exclusively at building
high-throughput, low-latency HTTP APIs. If robustness and performance
is more important to you than general purpose features, then Elli might be
for you. If you find yourself digging into the implementation of a
webserver, then Elli might be for you. If you're building web services,
not web sites, then Elli might be for you.

Elli requires OTP 22.0 or newer.

## Installation

Add `elli` to your application as a dependency to your
[`rebar.config`](https://www.rebar3.org/docs/configuration):

```erlang
{deps, [
  {elli, "3.3.0"}
]}.
```

Afterwards, to compile it, you can run:

```console
rebar3 compile
```

## Usage

To boot Elli inside an Erlang shell, run:

```console
rebar3 shell
```

```erlang
%% starting elli
1> {ok, Pid} = elli:start_link([{callback, elli_example_callback}, {port, 3000}]).
```

## Examples

### Callback Module

The best source to learn how to write a callback module
is [`elli_example_callback`](elli_example_callback.html).
There are also a bunch
of examples used in the tests as well as descriptions of all the events.

A minimal callback module looks something like this:

```erlang
-module(elli_minimal_callback).
-behaviour(elli_handler).

-include_lib("elli/include/elli.hrl").

-export([handle/2, handle_event/3]).

handle(Req, _Args) ->
    %% Delegate to our handler function
    Method = Req#req.method,
    Path = elli_request:path(Req),
    handle(Method, Path, Req).

handle('GET' = _Method, [<<"hello">>, <<"world">>] = _Path, _Req) ->
    %% Reply with a normal response. `ok' can be used instead of `200'
    %% to signal success.
    StatusCode = ok,
    Headers = [],
    Body = <<"Hello World!">>,
    {StatusCode, Headers, Body};

handle(_Method, _Path, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc Handle request events: request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
```

### Supervisor ChildSpec

To add `elli` to a supervisor you can use the following example and adapt it to
your needs.

```erlang
-module(elli_minimal_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    SupName = {local, ?MODULE},
    Module = ?MODULE,
    Args = [],
    supervisor:start_link(SupName, Module, Args).

init([] = _Args) ->
    ElliOpts = [
        {callback, elli_minimal_callback},
        {port, 3000}
    ],
    ElliSpec = {
        _Id = elli_minimal_http,
        _Start = {elli, start_link, [ElliOpts]},
        _Restart = permanent,
        _Shutdown = 5000,
        _Worker = worker,
        _Modules = [elli]},

    {ok, {{_Strategy = one_for_one, _Intensity = 5, _Period = 10}, [ElliSpec]} }.
```

## Further reading

For more information about the features and design philosophy of Elli check
out the [`overview`](overview.html).

## License

Elli is licensed under [The MIT License](LICENSE).

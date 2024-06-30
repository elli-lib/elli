# elli - Erlang web server for HTTP APIs

[![Hex.pm](https://img.shields.io/hexpm/v/elli.svg)](https://hex.pm/packages/elli)
[![Documentation](https://img.shields.io/badge/docs-edown-green.svg)](doc/README.md)
[![Erlang](https://img.shields.io/badge/erlang-%E2%89%A520.0-red.svg)](http://www.erlang.org/downloads)
![Common Test](https://github.com/elli-lib/elli/workflows/Common%20Test/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/elli-lib/elli/badge.svg?branch=develop)](https://coveralls.io/github/elli-lib/elli?branch=develop)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Elli is a webserver you can run inside your Erlang application to
expose an HTTP API. Elli is aimed exclusively at building
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
  {elli, "3.0.0"}
]}.
```

Afterwards you can run:

```console
rebar3 compile
```

## Usage

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
is [`elli_example_callback.erl`](elli_example_callback.html). There are a bunch
of examples used in the tests as well as descriptions of all the events.

A minimal callback module could look like this:

```erlang
-module(elli_minimal_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. `ok' can be used instead of `200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return `ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
```

### Supervisor Childspec

To add `elli` to a supervisor you can use the following example and adapt it to
your needs.

```erlang
-module(fancyapi_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ElliOpts = [{callback, fancyapi_callback}, {port, 3000}],
    ElliSpec = {
        fancy_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, { {one_for_one, 5, 10}, [ElliSpec]} }.
```

## Further Reading

For more information about the features and design philosophy of `elli` check
out the [overview](doc/README.md).

## License

Elli is licensed under [The MIT License](LICENSE).

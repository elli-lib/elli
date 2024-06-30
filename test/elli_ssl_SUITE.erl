-module(elli_ssl_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("test/support/elli_test.hrl").

-define(README, filename:join(code:priv_dir(elli), "README.md")).

-compile([export_all, nowarn_export_all]).

%
% Configuration.

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        not lists:member(Fun, [module_info, init_per_suite, end_per_suite,
                               get_size_value, get_timing_value,
                               status, body, headers])
    ].

init_per_suite(Config0) ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    {ok, StartedApps} = application:ensure_all_started(hackney),

    EbinDir  = filename:dirname(code:which(?MODULE)),
    CertDir  = filename:join([EbinDir, "pem"]),
    CertFile = filename:join(CertDir, "server_cert.pem"),
    KeyFile  = filename:join(CertDir, "server_key.pem"),

    Config = [
              {mods, [
                      {elli_metrics_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P}  = elli:start_link([
                                {port, 3443},
                                ssl,
                                {keyfile, KeyFile},
                                {certfile, CertFile},
                                {callback, elli_middleware},
                                {callback_args, Config}
                               ]),
    unlink(P),
    erlang:register(elli_under_test, P),
    [{pids, [P]}, {started_apps, StartedApps} | Config0].

end_per_suite(Config) ->
    Pids = proplists:get_value(pids, Config),
    [elli:stop(P) || P <- Pids],
    lists:foreach(fun (App) ->
                      application:stop(App)
                  end,
                  proplists:get_value(started_apps, Config)),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:start(asn1).

init_per_testcase(_Testcase, Config) ->
    ets:new(elli_stat_table, [set, named_table, public]),
    Config.

end_per_testcase(_Testcase, Config) ->
    ets:delete(elli_stat_table),
    Config.

%
% Tests.

hello_world(_Config) ->
    Response = hackney:get("https://localhost:3443/hello/world",
                           [], <<>>, [insecure]),
    ?assertMatch(200, status(Response)),
    ?assertMatch({ok, 200, _, _}, Response).

chunked(_Config) ->
    Expected = <<"chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1">>,

    Response = hackney:get("https://localhost:3443/chunked", [], <<>>, [insecure]),

    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-type">>, <<"text/event-stream">>},
                         {<<"transfer-encoding">>,<<"chunked">>}], headers(Response)),
    ?assertMatch(Expected, body(Response)).

sendfile(_Config) ->
    Response       = hackney:get("https://localhost:3443/sendfile", [], <<>>, [insecure]),
    F              = ?README,
    {ok, Expected} = file:read_file(F),

    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, ?I2B(size(Expected))}],
                 headers(Response)),
    ?assertEqual(Expected, body(Response)),
    %% sizes
    ?assertEqual(size(Expected), get_size_value(file)),
    ?assertMatch(65, get_size_value(resp_headers)),
    %% timings
    ?assertNotMatch(undefined, get_timing_value(request_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_end)),
    ?assertNotMatch(undefined, get_timing_value(body_start)),
    ?assertNotMatch(undefined, get_timing_value(body_end)),
    ?assertNotMatch(undefined, get_timing_value(user_start)),
    ?assertNotMatch(undefined, get_timing_value(user_end)),
    ?assertNotMatch(undefined, get_timing_value(send_start)),
    ?assertNotMatch(undefined, get_timing_value(send_end)),
    ?assertNotMatch(undefined, get_timing_value(request_end)).

acceptor_leak_regression(_Config) ->
    {ok, Before} = elli:get_acceptors(elli_under_test),
    Opts = [{verify, verify_peer},
            {verify_fun, {fun(_,_) -> {fail, 23} end, []}},
            {reuse_sessions, false}],
    {error, _} = ssl:connect("localhost", 3443, Opts),
    {ok, After} = elli:get_acceptors(elli_under_test),
    ?assertEqual(length(Before), length(After)).

%
% Private.

get_size_value(Key) ->
    [{sizes, Sizes}] = ets:lookup(elli_stat_table, sizes),
    proplists:get_value(Key, Sizes).

get_timing_value(Key) ->
    [{timings, Timings}] = ets:lookup(elli_stat_table, timings),
    proplists:get_value(Key, Timings).

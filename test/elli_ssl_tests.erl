-module(elli_ssl_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli_test.hrl").

-define(README, "README.md").

elli_ssl_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [{foreach,
       fun init_stats/0, fun clear_stats/1,
       [
        ?_test(hello_world()),
        ?_test(chunked()),
        ?_test(sendfile()),
        ?_test(acceptor_leak_regression()),
        ?_test(check_scheme_parsing())
       ]}
     ]}.

get_size_value(Key) ->
    [{sizes, Sizes}] = ets:lookup(elli_stat_table, sizes),
    proplists:get_value(Key, Sizes).

get_timing_value(Key) ->
    [{timings, Timings}] = ets:lookup(elli_stat_table, timings),
    proplists:get_value(Key, Timings).

%%% Tests

hello_world() ->
    Response = hackney:get("https://localhost:3443/hello/world",
                           [], <<>>, [insecure]),
    ?assertMatch(200, status(Response)),
    ?assertMatch({ok, 200, _, _}, Response).

check_scheme_parsing() ->
    Response = hackney:get("https://localhost:3443/scheme",
                           [], <<>>, [insecure]),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"https">>, body(Response)).

chunked() ->
    Expected = <<"chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1">>,

    Response = hackney:get("https://localhost:3443/chunked", [], <<>>, [insecure]),

    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-type">>, <<"text/event-stream">>},
                         {<<"transfer-encoding">>,<<"chunked">>}], headers(Response)),
    ?assertMatch(Expected, body(Response)).

sendfile() ->
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

acceptor_leak_regression() ->
    {ok, Before} = elli:get_acceptors(elli_under_test),
    Opts = [{verify, verify_peer},
            {verify_fun, {fun(_,_) -> {fail, 23} end, []}},
            {reuse_sessions, false}],
    {error, _} = ssl:connect("localhost", 3443, Opts),
    {ok, After} = elli:get_acceptors(elli_under_test),
    ?assertEqual(length(Before), length(After)).

%%% Internal helpers

setup() ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    {ok, _} = application:ensure_all_started(hackney),

    EbinDir  = filename:dirname(code:which(?MODULE)),
    CertDir  = filename:join([EbinDir, "..", "test"]),
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
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids],
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto).

init_stats() ->
    ets:new(elli_stat_table, [set, named_table, public]).

clear_stats(_) ->
    ets:delete(elli_stat_table).

-module(elli_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli_test.hrl").


elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(short_circuit()),
      ?_test(compress())
     ]}.

%%
%% TESTS
%%


short_circuit() ->
    URL            = "http://localhost:3002/middleware/short-circuit",
    {ok, Response} = httpc:request(URL),
    ?assertMatch("short circuit!", body(Response)).

hello_world() ->
    URL            = "http://localhost:3002/hello/world",
    {ok, Response} = httpc:request(URL),
    ?assertMatch("Hello World!", body(Response)).


compress() ->
    Url            = "http://localhost:3002/compressed",
    Headers        = [{"Accept-Encoding", "gzip"}],
    {ok, Response} = httpc:request(get, {Url, Headers}, [], []),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{"connection", "Keep-Alive"},
                  {"content-encoding", "gzip"},
                  {"content-length", "41"}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 zlib:gunzip(body(Response))),
    {ok, Response1} = httpc:request("http://localhost:3002/compressed"),
    ?assertMatch(200, status(Response1)),
    ?assertMatch([{"connection", "Keep-Alive"},
                  {"content-length", "1032"}], headers(Response1)),
    ?assertEqual(lists:flatten(lists:duplicate(86, "Hello World!")),
                 body(Response1)),
    Url2            = "http://localhost:3002/compressed-io_list",
    Headers2        = [{"Accept-Encoding", "gzip"}],
    {ok, Response2} = httpc:request(get, {Url2, Headers2}, [], []),
    ?assertMatch(200, status(Response2)),
    ?assertMatch([{"connection", "Keep-Alive"},
                  {"content-encoding", "gzip"},
                  {"content-length", "41"}], headers(Response2)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 zlib:gunzip(body(Response))),
    {ok, Response3} = httpc:request("http://localhost:3002/compressed-io_list"),
    ?assertMatch(200, status(Response3)),
    ?assertMatch([{"connection", "Keep-Alive"},
                  {"content-length", "1032"}], headers(Response3)),
    ?assertEqual(lists:flatten(lists:duplicate(86, "Hello World!")),
                 body(Response3)).

%%
%% HELPERS
%%

setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    Config = [
              {mods, [
                      {elli_access_log, [{name, elli_syslog},
                                         {ip, "127.0.0.1"},
                                         {port, 514}]},
                      {elli_example_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3002}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

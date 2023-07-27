-module(elli_middleware_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("test/support/elli_test.hrl").

-compile([export_all, nowarn_export_all]).

%
% Configuration.

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        not lists:member(Fun, [module_info, init_per_suite, end_per_suite,
                               status, body, headers])
    ].

init_per_suite(Config0) ->
    {ok, StartedApps} = application:ensure_all_started(hackney),

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

    {ok, P1} = elli:start_link([{callback, elli_middleware},
                                {callback_args, Config},
                                {port, 3002}]),
    unlink(P1),
    {ok, P2} = elli:start_link([{callback, elli_middleware},
                                {callback_args, [{mods, []}]},
                                {port, 3004}]),
    unlink(P2),
    [{pids, [P1, P2]}, {started_apps, StartedApps} | Config0].

end_per_suite(Config) ->
    Pids = proplists:get_value(pids, Config),
    [elli:stop(P) || P <- Pids],
    lists:foreach(fun (App) ->
                      application:stop(App)
                  end,
                  proplists:get_value(started_apps, Config)).

%
% Tests.

short_circuit(_Config) ->
    URL      = "http://localhost:3002/middleware/short-circuit",
    Response = hackney:get(URL),
    ?assertMatch(<<"short circuit!">>, body(Response)).

hello_world(_Config) ->
    URL      = "http://localhost:3002/hello/world",
    Response = hackney:get(URL),
    ?assertMatch(<<"Hello World!">>, body(Response)).

compress(_Config) ->
    URL      = "http://localhost:3002/compressed",
    Headers  = [{<<"Accept-Encoding">>, <<"gzip">>}],
    Response = hackney:get(URL, Headers),
    ?assertHeadersEqual([{<<"Connection">>, <<"Keep-Alive">>},
                         {<<"Content-Encoding">>, <<"gzip">>},
                         {<<"Content-Length">>, <<"41">>}],
                        headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 zlib:gunzip(body(Response))),
    Response1 = hackney:get("http://localhost:3002/compressed"),
    ?assertHeadersEqual([{<<"Connection">>, <<"Keep-Alive">>},
                         {<<"Content-Length">>, <<"1032">>}],
                        headers(Response1)),
    ?assertEqual(iolist_to_binary(lists:duplicate(86, "Hello World!")),
                 body(Response1)),
    Url2      = "http://localhost:3002/compressed-io_list",
    Headers2  = [{<<"Accept-Encoding">>, <<"gzip">>}],
    Response2 = hackney:get(Url2, Headers2),
    ?assertMatch(200, status(Response2)),
    ?assertHeadersEqual([{<<"Connection">>, <<"Keep-Alive">>},
                         {<<"Content-Encoding">>, <<"gzip">>},
                         {<<"Content-Length">>, <<"41">>}],
                        headers(Response2)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 zlib:gunzip(body(Response2))),
    Response3 = hackney:request("http://localhost:3002/compressed-io_list"),
    ?assertMatch(200, status(Response3)),
    ?assertHeadersEqual([{<<"Connection">>, <<"Keep-Alive">>},
                         {<<"Content-Length">>, <<"1032">>}],
                        headers(Response3)),
    ?assertEqual(iolist_to_binary(lists:duplicate(86, "Hello World!")),
                 body(Response3)).

no_callbacks(_Config) ->
    Response = hackney:get("http://localhost:3004/whatever"),
    ?assertMatch(404, status(Response)),
    ?assertMatch(<<"Not Found">>, body(Response)).

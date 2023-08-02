-module(elli_handover_SUITE).
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
                      {elli_example_callback_handover, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3003}]),
    unlink(P),
    [{pids, [P]}, {started_apps, StartedApps} | Config0].

end_per_suite(Config) ->
    Pids = proplists:get_value(pids, Config),
    [elli:stop(P) || P <- Pids],
    lists:foreach(fun (App) ->
                      application:stop(App)
                  end,
                  proplists:get_value(started_apps, Config)).

%
% Tests.

hello_world(_Config) ->
    Response = hackney:get("http://localhost:3003/hello/world"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"close">>},
                  {<<"Content-Length">>, <<"12">>}], headers(Response)),
    ?assertMatch(<<"Hello World!">>, body(Response)).

echo(_Config) ->
    Response = hackney:get("http://localhost:3003/hello?name=knut"),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"Hello knut">>, body(Response)).

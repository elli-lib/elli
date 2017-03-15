-module(elli_ssl_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli_test.hrl").

elli_ssl_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(chunked())
     ]}.

%%% Tests

%% FIXME
hello_world() ->
    {ok, Response} = httpc:request("https://localhost:3443/hello/world"),
    ?assertMatch(200, status(Response)).

%% FIXME
chunked() ->
    Expected = "chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1",

    {ok, Response} = httpc:request("https://localhost:3443/chunked"),

    ?assertMatch(200, status(Response)),
    ?assertEqual([{"connection", "Keep-Alive"},
                  %% httpc adds a content-length, even though elli
                  %% does not send any for chunked transfers
                  {"content-length", integer_to_list(length(Expected))},
                  {"content-type", "text/event-stream"}], headers(Response)),
    ?assertMatch(Expected, body(Response)).

%%% Internal helpers

setup() ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    EbinDir  = filename:dirname(code:which(?MODULE)),
    CertDir  = filename:join([EbinDir, "..", "test"]),
    CertFile = filename:join(CertDir, "server_cert.pem"),
    KeyFile  = filename:join(CertDir, "server_key.pem"),

    {ok, P}  = elli:start_link([
                                {port, 3443},
                                ssl,
                                {keyfile, KeyFile},
                                {certfile, CertFile},
                                {callback, elli_example_callback}
                               ]),
    unlink(P),
    [P].

teardown(Pids) ->
    inets:stop(),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    [elli:stop(P) || P <- Pids].

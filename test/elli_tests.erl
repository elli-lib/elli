-module(elli_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").

-define(I2B(I), list_to_binary(integer_to_list(I))).
-define(README, "README.md").

elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(not_found()),
      ?_test(crash()),
      ?_test(invalid_return()),
      ?_test(no_compress()),
      ?_test(exception_flow()),
      ?_test(accept_content_type()),
      ?_test(user_connection()),
      ?_test(get_args()),
      ?_test(decoded_get_args()),
      ?_test(decoded_get_args_list()),
      ?_test(post_args()),
      ?_test(shorthand()),
      ?_test(too_many_headers()),
      ?_test(too_big_body()),
      ?_test(way_too_big_body()),
      ?_test(bad_request_line()),
      ?_test(content_length()),
      ?_test(user_content_length()),
      ?_test(chunked()),
      %% FIXME: ?_test(sendfile()),
      %% FIXME: ?_test(sendfile_range()),
      ?_test(slow_client()),
      ?_test(post_pipeline()),
      ?_test(get_pipeline()),
      ?_test(head()),
      ?_test(no_body())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    {ok, P} = elli:start_link([{callback, elli_example_callback},
                               {port, 3001}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

%%% Integration tests
%%%   Use inets httpc to actually call Elli over the network.

hello_world() ->
    {ok, Response} = httpc:request("http://localhost:3001/hello/world"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "12"}], headers(Response)),
    ?assertMatch("Hello World!", body(Response)).



not_found() ->
    {ok, Response} = httpc:request("http://localhost:3001/foobarbaz"),
    ?assertMatch(404, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "9"}], headers(Response)),
    ?assertMatch("Not Found", body(Response)).

crash() ->
    {ok, Response} = httpc:request("http://localhost:3001/crash"),
    ?assertMatch(500, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "21"}], headers(Response)),
    ?assertMatch("Internal server error", body(Response)).

invalid_return() ->
    %% Elli should return 500 for handlers returning bogus responses.
    {ok, Response} = httpc:request("http://localhost:3001/invalid_return"),
    ?assertMatch(500, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "21"}], headers(Response)),
    ?assertMatch("Internal server error", body(Response)).

no_compress() ->
    {ok, Response} = httpc:request(get, {"http://localhost:3001/compressed",
                                         [{"Accept-Encoding", "gzip"}]},
                                   [], []),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "1032"}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 list_to_binary(body(Response))).

exception_flow() ->
    {ok, Response} = httpc:request("http://localhost:3001/403"),
    ?assertMatch(403, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "9"}], headers(Response)),
    ?assertMatch("Forbidden", body(Response)).

accept_content_type() ->
    {ok, Json} = httpc:request(get, {"http://localhost:3001/type?name=knut",
                                     [{"Accept", "application/json"}]}, [], []),
    ?assertMatch(<<"{\"name\" : \"knut\"}">>, list_to_binary(body(Json))),
    {ok, Text} = httpc:request(get, {"http://localhost:3001/type?name=knut",
                                     [{"Accept", "text/plain"}]}, [], []),
    ?assertMatch("name: knut", body(Text)).

user_connection() ->
    Url            = "http://localhost:3001/user/defined/behaviour",
    {ok, Response} = httpc:request(Url),
    ?assertMatch(304, status(Response)),
    ?assertMatch([{"connection", "close"},
                  {"content-length", "123"}], headers(Response)),
    ?assertMatch([], body(Response)).


get_args() ->
    {ok, Response} = httpc:request("http://localhost:3001/hello?name=knut"),
    ?assertMatch("Hello knut", body(Response)).

decoded_get_args() ->
    Url            = "http://localhost:3001/decoded-hello?name=knut%3D",
    {ok, Response} = httpc:request(Url),
    ?assertMatch("Hello knut=", body(Response)).

decoded_get_args_list() ->
    Url            = "http://localhost:3001/decoded-list?name=knut%3D&foo",
    {ok, Response} = httpc:request(Url),
    ?assertMatch("Hello knut=", body(Response)).

post_args() ->
    Body           = <<"name=foo&city=New%20York">>,
    ContentType    = "application/x-www-form-urlencoded",

    {ok, Response} = httpc:request(
                       post,
                       {"http://localhost:3001/hello", [], ContentType, Body},
                       [], []),
    ?assertMatch(200, status(Response)),
    ?assertMatch("Hello foo of New York", body(Response)).

shorthand() ->
    {ok, Response} = httpc:request("http://localhost:3001/shorthand"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "5"}], headers(Response)),
    ?assertMatch("hello", body(Response)).


too_many_headers() ->
    Headers = lists:duplicate(100, {"X-Foo", "Bar"}),
    {ok, Response} = httpc:request(get, {"http://localhost:3001/foo", Headers},
                                   [], []),
    ?assertMatch(400, status(Response)).

too_big_body() ->
    Body = binary:copy(<<"x">>, (1024 * 1000) + 1),
    {ok, Response} = httpc:request(post,
                                   {"http://localhost:3001/foo", [], [], Body},
                                   [], []),
    ?assertMatch(413, status(Response)).

way_too_big_body() ->
    Body = binary:copy(<<"x">>, (1024 * 2000) + 1),
    ?assertMatch({error, socket_closed_remotely},
                 httpc:request(post,
                               {"http://localhost:3001/foo", [], [], Body},
                               [], [])).


bad_request_line() ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),

    Req = <<"FOO BAR /hello HTTP/1.1\r\n">>,
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
    ?assertMatch({ok, <<"HTTP/1.1 400 Bad Request\r\n"
                        "Content-Length: 11\r\n\r\n">>},
                 gen_tcp:recv(Socket, 0)).


content_length() ->
    {ok, Response} = httpc:request("http://localhost:3001/304"),

    ?assertMatch(304, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "7"},
                  {"etag", "foobar"}], headers(Response)),
    ?assertMatch([], body(Response)).

user_content_length() ->
    Headers = <<"Foo: bar\n\n">>,
    Client  = start_slow_client(3001, "/user/content-length"),
    send(Client, Headers, 128),
    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n"
                        "Connection: keep-alive\r\n"
                        "Content-Length: 123\r\n"
                        "\r\n"
                        "foobar">>},
                 gen_tcp:recv(Client, 0)).


chunked() ->
    Expected = "chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1",

    {ok, Response} = httpc:request("http://localhost:3001/chunked"),
    ?assertMatch(200, status(Response)),
    ?assertEqual([{"connection", "keep-alive"},
                  %% httpc adds a content-length, even though elli
                  %% does not send any for chunked transfers
                  {"content-length", integer_to_list(length(Expected))},
                  {"content-type", "text/event-stream"}], headers(Response)),
    ?assertMatch(Expected, body(Response)).

%% sendfile() ->
%%   {ok, Response} = httpc:request("http://localhost:3001/sendfile"),
%%   F              = ?README,
%%   {ok, Expected} = file:read_file(F),
%%   ?assertMatch(200, status(Response)),
%%   ?assertMatch([{"connection", "keep-alive"},
%%                 {"content-length", integer_to_list(size(Expected))}],
%%                headers(Response)),
%%   ?assertEqual(binary_to_list(Expected), body(Response)).

%% sendfile_range() ->
%%   Url            = "http://localhost:3001/sendfile/range",
%%   Headers        = [{"Range", "bytes=300-699"}],
%%   {ok, Response} = httpc:request(get, {Url, Headers}, [], []),
%%   F              = ?README,
%%   {ok, Fd}       = file:open(F, [read, raw, binary]),
%%   {ok, Expected} = file:pread(Fd, 300, 400),
%%   file:close(Fd),
%%   Size = elli_util:file_size(F),
%%   ?assertMatch(206, status(Response)),
%%   ?assertEqual([{"connection", "keep-alive"},
%%                 {"content-length", "400"},
%%                 {"content-range", "bytes 300-699/" ++ ?I2L(Size)}],
%%                headers(Response)),
%%   ?assertEqual(binary_to_list(Expected), body(Response)).

slow_client() ->
    Body    = <<"name=foobarbaz">>,
    Headers = <<"Content-Length: ", (?I2B(size(Body)))/binary, "\r\n\r\n">>,
    Client  = start_slow_client(3001, "/hello"),

    send(Client, Headers, 1),
    send(Client, Body, size(Body)),

    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n"
                        "Connection: keep-alive\r\n"
                        "Content-Length: 15\r\n"
                        "\r\n"
                        "Hello undefined">>},
                 gen_tcp:recv(Client, 0)).


post_pipeline() ->
    Body         = <<"name=elli&city=New%20York">>,
    Headers      = <<"Content-Length: ", (?I2B(size(Body)))/binary, "\r\n",
                     "Content-Type: application/x-www-form-urlencoded", "\r\n",
                     "\r\n">>,

    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),

    Req          = <<"POST /hello HTTP/1.1\r\n",
                     Headers/binary,
                     Body/binary>>,
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),

    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "Connection: keep-alive\r\n"
                         "Content-Length: 22\r\n"
                         "\r\n"
                         "Hello elli of New York">>,

    {ok, Res} = gen_tcp:recv(Socket, size(ExpectedResponse) * 2),

    ?assertEqual(binary:copy(ExpectedResponse, 2),
                 Res).

get_pipeline() ->
    Headers      = <<"User-Agent: sloow\r\n\r\n">>,
    Req          = <<"GET /hello?name=elli HTTP/1.1\r\n",
                     Headers/binary>>,
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "Connection: keep-alive\r\n"
                         "Content-Length: 10\r\n"
                         "\r\n"
                         "Hello elli">>,
    {ok, Res} = gen_tcp:recv(Socket, size(ExpectedResponse) * 2),
    case binary:copy(ExpectedResponse, 2) =:= Res of
        true ->
            ok;
        false ->
            error_logger:info_msg("Expected: ~p~nResult: ~p~n",
                                  [binary:copy(ExpectedResponse, 2), Res])
    end,

    ?assertEqual(binary:copy(ExpectedResponse, 2),
                 Res).

head() ->
    {ok, Response} = httpc:request(head, {"http://localhost:3001/head", []},
                                   [], []),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "20"}], headers(Response)),
    ?assertMatch([], body(Response)).


no_body() ->
    {ok, Response} = httpc:request("http://localhost:3001/304"),
    ?assertMatch(304, status(Response)),
    ?assertMatch([{"connection", "keep-alive"},
                  {"content-length", "7"},
                  {"etag", "foobar"}], headers(Response)),
    ?assertMatch([], body(Response)).

%%% Slow client, sending only the specified byte size every millisecond

start_slow_client(Port, Url) ->
    case gen_tcp:connect("127.0.0.1", Port, [{active, false}, binary]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, "GET " ++ Url ++ " HTTP/1.1\r\n"),
            Socket;
        {error, Reason} ->
            throw({slow_client_error, Reason})
    end.

send(_Socket, <<>>, _) ->
    ok;
send(Socket, B, ChunkSize) ->
    {Part, Rest} = case B of
                       <<P:ChunkSize/binary, R/binary>> -> {P, R};
                       P -> {P, <<>>}
                   end,
    %%error_logger:info_msg("~p~n", [Part]),
    gen_tcp:send(Socket, Part),
    timer:sleep(1),
    send(Socket, Rest, ChunkSize).

%%% Unit tests

body_qs_test() ->
    Expected = [{<<"foo">>, <<"bar">>},
                {<<"baz">>, <<"bang">>},
                {<<"found">>, true}],
    Body     = <<"foo=bar&baz=bang&found">>,
    Headers  = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    ?assertMatch(Expected, elli_request:body_qs(#req{body = Body,
                                                     headers = Headers})).

to_proplist_test() ->
    Req  = #req{method   = 'GET',
                path     = [<<"crash">>],
                args     = [],
                version  = {1, 1},
                raw_path = <<"/crash">>,
                headers  = [{<<"Host">>, <<"localhost:3001">>}],
                body     = <<>>,
                pid      = self(),
                socket   = socket,
                callback = {mod, []}},

    Prop = [{method,   'GET'},
            {path,     [<<"crash">>]},
            {args,     []},
            {raw_path, <<"/crash">>},
            {version,  {1, 1}},
            {headers,  [{<<"Host">>, <<"localhost:3001">>}]},
            {body,     <<>>},
            {pid,      self()},
            {socket,   socket},
            {callback, {mod, []}}],
    ?assertEqual(Prop, elli_request:to_proplist(Req)).

is_request_test() ->
    ?assert(elli_request:is_request(#req{})),
    ?assertNot(elli_request:is_request({req, foobar})).


query_str_test_() ->
    MakeReq = fun(Path) -> #req{raw_path = Path} end,
    [
     %% For empty query strings, expect `query_str` to return an empty binary.
     ?_assertMatch(<<>>, elli_request:query_str(MakeReq(<<"/foo">>))),
     ?_assertMatch(<<>>, elli_request:query_str(MakeReq(<<"/foo?">>))),
     %% Otherwise it should return everything to the right hand side of `?`.
     ?_assertMatch(<<"bar=baz&baz=bang">>,
                   elli_request:query_str(MakeReq(<<"/foo?bar=baz&baz=bang">>)))
    ].


get_range_test_() ->
    Req       = #req{headers = [{<<"Range">>,
                                 <<"bytes=0-99 ,500-999 , -800">>}]},
    OffsetReq = #req{headers = [{<<"Range">>, <<"bytes=200-">>}]},
    UndefReq  = #req{headers = []},
    BadReq    = #req{headers = [{<<"Range">>, <<"bytes=--99,hallo-world">>}]},

    ByteRangeSet = [{bytes, 0, 99}, {bytes, 500, 999}, {suffix, 800}],

    [?_assertMatch(ByteRangeSet,    elli_request:get_range(Req)),
     ?_assertMatch([{offset, 200}], elli_request:get_range(OffsetReq)),
     ?_assertMatch([],              elli_request:get_range(UndefReq)),
     ?_assertMatch(parse_error,     elli_request:get_range(BadReq))].

normalize_range_test_() ->
    Size     = 1000,

    Bytes1   = {bytes, 200, 400},
    Bytes2   = {bytes, 0, 1000000},
    Suffix   = {suffix, 303},
    Offset   = {offset, 42},
    Normal   = {200, 400},
    Set      = [{bytes, 0, 999}],
    EmptySet = [],
    Invalid1 = {bytes, 400, 200},
    Invalid2 = {bytes, 1200, 2000},
    Invalid3 = {offset, -10},
    Invalid4 = {offset, 2000},
    Invalid5 = parse_error,
    Invalid6 = [{bytes, 0, 100}, {suffix, 42}],

    [?_assertMatch({200, 201},        elli_util:normalize_range(Bytes1, Size)),
     ?_assertMatch({0, Size},         elli_util:normalize_range(Bytes2, Size)),
     ?_assertEqual({Size - 303, 303}, elli_util:normalize_range(Suffix, Size)),
     ?_assertEqual({42, Size - 42},   elli_util:normalize_range(Offset, Size)),
     ?_assertMatch({200, 400},        elli_util:normalize_range(Normal, Size)),
     ?_assertMatch({0, 1000},         elli_util:normalize_range(Set, Size)),
     ?_assertMatch(undefined,     elli_util:normalize_range(EmptySet, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid1, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid2, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid3, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid4, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid5, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid6, Size))].


register_test() ->
    ?assertMatch(undefined, whereis(elli)),
    {ok, Pid} = elli:start_link([
                                 {name, {local, elli}},
                                 {callback, elli_example_callback}
                                ]),
    ?assertMatch(Pid, whereis(elli)),
    ok.

invalid_callback_test() ->
    case catch elli:start_link([{callback, elli}]) of
        E ->
            ?assertMatch(invalid_callback, E)
    end.


%%% Helpers

status({{_, Status, _}, _, _}) ->
    Status.
body({_, _, Body}) ->
    Body.

headers({_, Headers, _}) ->
    lists:sort(Headers).

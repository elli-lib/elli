-module(elli_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("elli.hrl").
-include("test/support/elli_test.hrl").

-compile([export_all, nowarn_export_all]).

-define(README, filename:join(code:priv_dir(elli), "README.md")).
-define(VTB(T1, T2, LB, UB),
        time_diff_to_micro_seconds(T1, T2) >= LB andalso
        time_diff_to_micro_seconds(T1, T2) =< UB).
-include_lib("kernel/include/logger.hrl").

%
% Configuration.

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        not lists:member(Fun, [module_info, init_per_suite, end_per_suite,
                               get_timing_value, get_size_value,
                               status, body, headers])
    ].

init_per_suite(Config0) ->
    {ok, StartedApps} = application:ensure_all_started(hackney),

    Config = [
              {mods, [
                      {elli_metrics_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_example_callback, []}
                     ]}
             ],
    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3001}]),
    unlink(P),
    [{pids, [P]}, {started_apps, StartedApps} | Config0].

end_per_suite(Config) ->
    Pids = proplists:get_value(pids, Config),
    [elli:stop(P) || P <- Pids],
    lists:foreach(fun (App) ->
                      application:stop(App)
                  end,
                  proplists:get_value(started_apps, Config)).

init_per_testcase(_Testcase, Config) ->
    ets:new(elli_stat_table, [set, named_table, public]),
    Config.

end_per_testcase(_Testcase, Config) ->
    ets:delete(elli_stat_table),
    Config.

%
% Tests.

accessors(_Config) ->
    RawPath = <<"/foo/bar">>,
    Headers = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Method = 'POST',
    Body = <<"name=knut%3D">>,
    Name = <<"knut=">>,
    Req1 = #req{raw_path = RawPath,
                original_headers = Headers,
                headers = Headers,
                method = Method,
                body = Body},
    Args = [{<<"name">>, Name}],
    Req2 = #req{original_headers = Headers, headers = Headers, args = Args, body = <<>>},

    [
     %% POST /foo/bar
     ?assertMatch(RawPath, elli_request:raw_path(Req1)),
     ?assertMatch(Headers, elli_request:headers(Req1)),
     ?assertMatch(Method, elli_request:method(Req1)),
     ?assertMatch(Body, elli_request:body(Req1)),
     ?assertMatch(Args, elli_request:post_args_decoded(Req1)),
     ?assertMatch(undefined, elli_request:post_arg(<<"foo">>, Req1)),
     ?assertMatch(undefined, elli_request:post_arg_decoded(<<"foo">>, Req1)),
     ?assertMatch(Name, elli_request:post_arg_decoded(<<"name">>, Req1)),
     %% GET /foo/bar
     ?assertMatch(Headers, elli_request:headers(Req2)),

     ?assertMatch(Args, elli_request:get_args(Req2)),
     ?assertMatch(undefined, elli_request:get_arg_decoded(<<"foo">>, Req2)),
     ?assertMatch(Name, elli_request:get_arg_decoded(<<"name">>, Req2)),
     ?assertMatch([], elli_request:post_args(Req2)),

     ?assertMatch({error, not_supported}, elli_request:chunk_ref(#req{}))
    ].

hello_world(_Config) ->
    Response = hackney:get("http://localhost:3001/hello/world"),
    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"12">>}], headers(Response)),
    ?assertMatch(<<"Hello World!">>, body(Response)),
    %% sizes
    ?assertMatch(63, get_size_value(resp_headers)),
    ?assertMatch(12, get_size_value(resp_body)),
    ?assertMatch(undefined, get_size_value(req_body)),

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
    ?assertNotMatch(undefined, get_timing_value(request_end)),
    %% check timings
    ?assert(?VTB(request_start, request_end, 1000000, 1200000)),
    ?assert(?VTB(headers_start, headers_end, 1, 100)),
    ?assert(?VTB(body_start, body_end, 1, 100)),
    ?assert(?VTB(user_start, user_end, 1000000, 1200000)),
    ?assert(?VTB(send_start, send_end, 1, 2000)).

keep_alive_timings(_Config) ->

    Transport = hackney_tcp,
    Host = <<"localhost">>,
    Port = 3001,
    Options = [],
    {ok, ConnRef} = hackney:connect(Transport, Host, Port, Options),

    ReqBody = <<>>,
    ReqHeaders = [],
    ReqPath = <<"/hello/world">>,
    ReqMethod = get,
    Req = {ReqMethod, ReqPath, ReqHeaders, ReqBody},

    {ok, Status, Headers, HCRef} = hackney:send_request(ConnRef, Req),
    keep_alive_timings(Status, Headers, HCRef),

    %% pause between keep-alive requests,
    %% request_start is a timestamp of
    %% the first bytes of the second request
    timer:sleep(1000),

    {ok, Status, Headers, HCRef} = hackney:send_request(ConnRef, Req),
    keep_alive_timings(Status, Headers, HCRef),

    hackney:close(ConnRef).

not_found(_Config) ->
    Response = hackney:get("http://localhost:3001/foobarbaz"),
    ?assertMatch(404, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                  {<<"content-length">>, <<"9">>}], headers(Response)),
    ?assertMatch(<<"Not Found">>, body(Response)).

crash(_Config) ->
    Response = hackney:get("http://localhost:3001/crash"),
    ?assertMatch(500, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                  {<<"content-length">>, <<"21">>}], headers(Response)),
    ?assertMatch(<<"Internal server error">>, body(Response)).

invalid_return(_Config) ->
    %% Elli should return 500 for handlers returning bogus responses.
    Response = hackney:get("http://localhost:3001/invalid_return"),
    ?assertMatch(500, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                  {<<"content-length">>, <<"21">>}], headers(Response)),
    ?assertMatch(<<"Internal server error">>, body(Response)).

no_compress(_Config) ->
    Response = hackney:get("http://localhost:3001/compressed"),
    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                  {<<"content-length">>, <<"1032">>}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 body(Response)).

gzip(_Config) -> compress(<<"gzip">>, <<"41">>).

deflate(_Config) -> compress(<<"deflate">>, <<"29">>).

exception_flow(_Config) ->
    Response = hackney:get("http://localhost:3001/403"),
    ?assertMatch(403, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                  {<<"content-length">>, <<"9">>}], headers(Response)),
    ?assertMatch(<<"Forbidden">>, body(Response)).

hello_iolist(_Config) ->
    Url      = "http://localhost:3001/hello/iolist?name=knut",
    Response = hackney:get(Url),
    ?assertMatch(<<"Hello knut">>, body(Response)).

accept_content_type(_Config) ->
    Json = hackney:get("http://localhost:3001/type?name=knut",
                       [{"Accept", "application/json"}]),
    ?assertMatch(<<"{\"name\" : \"knut\"}">>, body(Json)),
    Text = hackney:get("http://localhost:3001/type?name=knut",
                       [{"Accept", "text/plain"}]),
    ?assertMatch(<<"name: knut">>, body(Text)).

user_connection(_Config) ->
    Url      = "http://localhost:3001/user/defined/behaviour",
    Response = hackney:get(Url),
    ?assertMatch(304, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"close">>},
                         {<<"content-length">>, <<"123">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

get_args(_Config) ->
    Response = hackney:get("http://localhost:3001/hello?name=knut"),
    ?assertMatch(<<"Hello knut">>, body(Response)).

decoded_get_args(_Config) ->
    Url      = "http://localhost:3001/decoded-hello?name=knut%3D",
    Response = hackney:get(Url),
    ?assertMatch(<<"Hello knut=">>, body(Response)).

decoded_get_args_list(_Config) ->
    Url      = "http://localhost:3001/decoded-list?name=knut%3D&foo",
    Response = hackney:get(Url),
    ?assertMatch(<<"Hello knut=">>, body(Response)).

post_args(_Config) ->
    Body        = <<"name=foo&city=New%20York">>,
    ContentType = <<"application/x-www-form-urlencoded">>,

    Response    = hackney:post("http://localhost:3001/hello",
                               [{<<"content-type">>, ContentType}],
                               Body),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"Hello foo of New York">>, body(Response)).

shorthand(_Config) ->
    Response = hackney:get("http://localhost:3001/shorthand"),
    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"5">>}], headers(Response)),
    ?assertMatch(<<"hello">>, body(Response)).

ip(_Config) ->
    Response = hackney:get("http://localhost:3001/ip"),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"127.0.0.1">>, body(Response)).

found(_Config) ->
    Response = hackney:get("http://localhost:3001/302"),
    ?assertMatch(302, status(Response)),
    ?assertHeadersEqual([{<<"Location">>, <<"/hello/world">>},
                         {<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"0">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

too_many_headers(_Config) ->
    Headers = lists:duplicate(100, {<<"X-Foo">>, <<"Bar">>}),
    Response = hackney:get("http://localhost:3001/foo", Headers),
    ?assertMatch(400, status(Response)).

too_big_body(_Config) ->
    Body = binary:copy(<<"x">>, (1024 * 1000) + 1),
    Response = hackney:post("http://localhost:3001/foo", [], Body),
    ?assertMatch(413, status(Response)).

way_too_big_body(_Config) ->
    Body = binary:copy(<<"x">>, (1024 * 2000) + 1),
    ?assertMatch({error, closed},
                 hackney:post("http://localhost:3001/foo", [], Body)).

bad_request_line(_Config) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),

    Req = <<"FOO BAR /hello HTTP/1.1\r\n">>,
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
    ?assertMatch({ok, <<"HTTP/1.1 400 Bad Request\r\n"
                        "content-length: 11\r\n\r\nBad Request">>},
                 gen_tcp:recv(Socket, 0)).

content_length(_Config) ->
    Response = hackney:get("http://localhost:3001/304"),

    ?assertMatch(304, status(Response)),
    ?assertHeadersEqual([{<<"Etag">>, <<"foobar">>},
                         {<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"7">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

user_content_length(_Config) ->
    Headers = <<"Foo: bar\n\n">>,
    Client  = start_slow_client(3001, "/user/content-length"),
    send(Client, Headers, 128),
    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n"
                        "connection: Keep-Alive\r\n"
                        "Content-Length: 123\r\n"
                        "\r\n"
                        "foobar">>},
                 gen_tcp:recv(Client, 0)).

headers_(_Config) ->
    Response = hackney:get("http://localhost:3001/headers.html"),
    Headers = headers(Response),

    ?assert(proplists:is_defined(<<"X-Custom">>, Headers)),
    ?assertMatch(<<"foobar">>, proplists:get_value(<<"X-Custom">>, Headers)).

chunked(_Config) ->
    Expected = <<"chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1">>,

    Response = hackney:get("http://localhost:3001/chunked"),

    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-type">>, <<"text/event-stream">>},
                         {<<"transfer-encoding">>, <<"chunked">>}], headers(Response)),
    ?assertMatch(Expected, body(Response)),
    %% sizes
    ?assertMatch(104, get_size_value(resp_headers)),
    ?assertMatch(111, get_size_value(chunks)),
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

sendfile(_Config) ->
    Response = hackney:get("http://localhost:3001/sendfile"),
    F              = ?README,
    {ok, Expected} = file:read_file(F),

    ?assertMatch(200, status(Response)),
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

send_no_file(_Config) ->
    Response = hackney:get("http://localhost:3001/send_no_file"),

    ?assertMatch(500, status(Response)),
    ?assertHeadersEqual([{<<"content-length">>, <<"12">>}],
                 headers(Response)),
    ?assertMatch(<<"Server Error">>, body(Response)).

sendfile_error(_Config) ->
    Response = hackney:get("http://localhost:3001/sendfile/error"),

    ?assertMatch(500, status(Response)),
    ?assertHeadersEqual([{<<"content-length">>, <<"12">>}],
                 headers(Response)),
    ?assertMatch(<<"Server Error">>, body(Response)).

sendfile_range(_Config) ->
    Url            = "http://localhost:3001/sendfile/range",
    Headers        = [{"Range", "bytes=300-699"}],
    Response       = hackney:get(Url, Headers),
    F              = ?README,
    {ok, Fd}       = file:open(F, [read, raw, binary]),
    {ok, Expected} = file:pread(Fd, 300, 400),
    file:close(Fd),
    Size = elli_util:file_size(F),
    ?assertMatch(206, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"400">>},
                         {<<"Content-Range">>,
                          iolist_to_binary(["bytes 300-699/", integer_to_binary(Size)])}],
                        headers(Response)),
    ?assertEqual(Expected, body(Response)).

slow_client(_Config) ->
    Body    = <<"name=foobarbaz">>,
    Headers = <<"content-length: ", (?I2B(size(Body)))/binary, "\r\n\r\n">>,
    Client  = start_slow_client(3001, "/hello"),

    send(Client, Headers, 1),
    send(Client, Body, size(Body)),

    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n"
                        "connection: Keep-Alive\r\n"
                        "content-length: 15\r\n"
                        "\r\n"
                        "Hello undefined">>},
                 gen_tcp:recv(Client, 0)),
    %% check timings
    ?assert(?VTB(request_start, request_end, 30000, 70000)),
    ?assert(?VTB(headers_start, headers_end, 30000, 70000)),
    ?assert(?VTB(body_start, body_end, 1, 3000)),
    ?assert(?VTB(user_start, user_end, 1, 100)),
    ?assert(?VTB(send_start, send_end, 1, 200)).

post_pipeline(_Config) ->
    Body         = <<"name=elli&city=New%20York">>,
    Headers      = <<"content-length: ", (?I2B(size(Body)))/binary, "\r\n",
                     "Content-Type: application/x-www-form-urlencoded", "\r\n",
                     "\r\n">>,

    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),

    Req          = <<"POST /hello HTTP/1.1\r\n",
                     Headers/binary,
                     Body/binary>>,
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),

    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "connection: Keep-Alive\r\n"
                         "content-length: 22\r\n"
                         "\r\n"
                         "Hello elli of New York">>,

    {ok, Res} = gen_tcp:recv(Socket, size(ExpectedResponse) * 2),

    Size = size(Body),
    ?assertMatch(Size, get_size_value(req_body)),
    ?assertEqual(binary:copy(ExpectedResponse, 2),
                 Res).

get_pipeline(_Config) ->
    Headers      = <<"User-Agent: sloow\r\n\r\n">>,
    Req          = <<"GET /hello?name=elli HTTP/1.1\r\n",
                     Headers/binary>>,
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "connection: Keep-Alive\r\n"
                         "content-length: 10\r\n"
                         "\r\n"
                         "Hello elli">>,
    {ok, Res} = gen_tcp:recv(Socket, size(ExpectedResponse) * 2),
    case binary:copy(ExpectedResponse, 2) =:= Res of
        true ->
            ok;
        false ->
            ?LOG_INFO("Expected: ~p~nResult: ~p~n",
                      [binary:copy(ExpectedResponse, 2), Res])
    end,

    ?assertEqual(binary:copy(ExpectedResponse, 2),
                 Res).

head(_Config) ->
    Response = hackney:head("http://localhost:3001/head"),
    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"20">>}], headers(Response)).

no_body(_Config) ->
    Response = hackney:get("http://localhost:3001/304"),
    ?assertMatch(304, status(Response)),
    ?assertHeadersEqual([{<<"connection">>, <<"Keep-Alive">>},
                         {<<"content-length">>, <<"7">>},
                         {<<"Etag">>, <<"foobar">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

sends_continue(_Config) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001, [{active, false}, binary]),

    Body = <<"name=elli&city=New%20York">>,
    Length = ?I2B(size(Body)),

    Req = <<"POST /hello HTTP/1.1\r\n",
            "Host: localhost\r\n",
            "Content-Type: application/x-www-form-urlencoded\r\n",
            "content-length: ", Length/binary, "\r\n",
            "Expect: 100-continue\r\n\r\n">>,

    gen_tcp:send(Socket, Req),
    ?assertMatch({ok, <<"HTTP/1.1 100 Continue\r\n"
                        "content-length: 0\r\n\r\n">>},
                 gen_tcp:recv(Socket, 0)),
    % Send Result of the body
    gen_tcp:send(Socket, Body),
    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "connection: Keep-Alive\r\n"
                         "content-length: 22\r\n"
                         "\r\n"
                         "Hello elli of New York">>,
    ?assertMatch({ok, ExpectedResponse},
                 gen_tcp:recv(Socket, size(ExpectedResponse))).

body_qs(_Config) ->
    Expected = [{<<"foo">>, <<"bar">>},
                {<<"baz">>, <<"bang">>},
                {<<"found">>, true}],
    Body     = <<"foo=bar&baz=bang&found">>,
    Headers  = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    ?assertMatch(Expected, elli_request:body_qs(#req{body = Body,
                                                     original_headers = Headers,
                                                     headers = Headers})).

to_proplist(_Config) ->
    Req  = #req{method   = 'GET',
                path     = [<<"crash">>],
                args     = [],
                version  = {1, 1},
                raw_path = <<"/crash">>,
                original_headers  = [{<<"Host">>, <<"localhost:3001">>}],
                headers  = [{<<"host">>, <<"localhost:3001">>}],
                body     = <<>>,
                pid      = self(),
                socket   = socket,
                callback = {mod, []}},

    Prop = [{method,   'GET'},
            {scheme,   undefined},
            {host,     undefined},
            {port,     undefined},
            {path,     [<<"crash">>]},
            {args,     []},
            {raw_path, <<"/crash">>},
            {version,  {1, 1}},
            {headers,  [{<<"host">>, <<"localhost:3001">>}]},
            {original_headers,  [{<<"Host">>, <<"localhost:3001">>}]},
            {body,     <<>>},
            {pid,      self()},
            {socket,   socket},
            {callback, {mod, []}}],
    ?assertEqual(Prop, elli_request:to_proplist(Req)).

is_request(_Config) ->
    ?assert(elli_request:is_request(#req{})),
    ?assertNot(elli_request:is_request({req, foobar})).

query_str(_Config) ->
    MakeReq = fun(Path) -> #req{raw_path = Path} end,
    [
     %% For empty query strings, expect `query_str` to return an empty binary.
     ?assertMatch(<<>>, elli_request:query_str(MakeReq(<<"/foo">>))),
     ?assertMatch(<<>>, elli_request:query_str(MakeReq(<<"/foo?">>))),
     %% Otherwise it should return everything to the right hand side of `?`.
     ?assertMatch(<<"bar=baz&baz=bang">>,
                   elli_request:query_str(MakeReq(<<"/foo?bar=baz&baz=bang">>)))
    ].

get_range(_Config) ->
    Req       = #req{headers = [{<<"range">>,
                                        <<"bytes=0-99 ,500-999 , -800">>}]},
    OffsetReq = #req{headers = [{<<"range">>, <<"bytes=200-">>}]},
    UndefReq  = #req{headers = []},
    BadReq    = #req{headers = [{<<"range">>, <<"bytes=--99,hallo-world">>}]},

    ByteRangeSet = [{bytes, 0, 99}, {bytes, 500, 999}, {suffix, 800}],

    [?assertMatch(ByteRangeSet,    elli_request:get_range(Req)),
     ?assertMatch([{offset, 200}], elli_request:get_range(OffsetReq)),
     ?assertMatch([],              elli_request:get_range(UndefReq)),
     ?assertMatch(parse_error,     elli_request:get_range(BadReq))].

normalize_range(_Config) ->
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

    [?assertMatch({200, 201},        elli_util:normalize_range(Bytes1, Size)),
     ?assertMatch({0, Size},         elli_util:normalize_range(Bytes2, Size)),
     ?assertEqual({Size - 303, 303}, elli_util:normalize_range(Suffix, Size)),
     ?assertEqual({42, Size - 42},   elli_util:normalize_range(Offset, Size)),
     ?assertMatch({200, 400},        elli_util:normalize_range(Normal, Size)),
     ?assertMatch({0, 1000},         elli_util:normalize_range(Set, Size)),
     ?assertMatch(undefined,     elli_util:normalize_range(EmptySet, Size)),
     ?assertMatch(invalid_range, elli_util:normalize_range(Invalid1, Size)),
     ?assertMatch(invalid_range, elli_util:normalize_range(Invalid2, Size)),
     ?assertMatch(invalid_range, elli_util:normalize_range(Invalid3, Size)),
     ?assertMatch(invalid_range, elli_util:normalize_range(Invalid4, Size)),
     ?assertMatch(invalid_range, elli_util:normalize_range(Invalid5, Size)),
     ?assertMatch(invalid_range, elli_util:normalize_range(Invalid6, Size))].

encode_range(_Config) ->
    Expected = [<<"bytes ">>,<<"*">>,<<"/">>,<<"42">>],
    ?assertMatch(Expected, elli_util:encode_range(invalid_range, 42)).

register(_Config) ->
    ?assertMatch(undefined, whereis(elli)),
    Config = [
              {name, {local, elli}},
              {callback, elli_middleware},
              {callback_args, [{mods, [
                                       elli_example_callback,
                                       elli_metrics_middleware
                                      ]}]}
             ],
    {ok, Pid} = elli:start_link(Config),
    ?assertMatch(Pid, whereis(elli)),
    ok.

invalid_callback(_Config) ->
    try
        elli:start_link([{callback, elli}])
    catch E ->
        ?assertMatch(invalid_callback, E)
    end.

hello_world2(_Config) ->
    ?assertMatch({ok, [], <<"Hello World!">>},
                 elli_test:call('GET', <<"/hello/world/">>, [], <<>>,
                                ?EXAMPLE_CONF)),
    ?assertMatch({ok, [], <<"Hello Test1">>},
                 elli_test:call('GET', <<"/hello/?name=Test1">>, [], <<>>,
                                ?EXAMPLE_CONF)),
    ?assertMatch({ok,
                  [{<<"content-type">>,
                    <<"application/json; charset=ISO-8859-1">>}],
                  <<"{\"name\" : \"Test2\"}">>},
                 elli_test:call('GET', <<"/type?name=Test2">>,
                                [{<<"accept">>, <<"application/json">>}], <<>>,
                                ?EXAMPLE_CONF)).

%
% Private.

time_diff_to_micro_seconds(T1, T2) ->
    erlang:convert_time_unit(
      get_timing_value(T2) -
          get_timing_value(T1),
      native,
      micro_seconds).

get_timing_value(Key) ->
    [{timings, Timings}] = ets:lookup(elli_stat_table, timings),
    proplists:get_value(Key, Timings).

get_size_value(Key) ->
    [{sizes, Sizes}] = ets:lookup(elli_stat_table, sizes),
    proplists:get_value(Key, Sizes).

keep_alive_timings(Status, Headers, HCRef) ->
    ?assertMatch(200, Status),
    ?assertHeadersEqual([{<<"connection">>,<<"Keep-Alive">>},
                         {<<"content-length">>,<<"12">>}], Headers),
    ?assertMatch({ok, <<"Hello World!">>}, hackney:body(HCRef)),
    %% sizes
    ?assertMatch(63, get_size_value(resp_headers)),
    ?assertMatch(12, get_size_value(resp_body)),
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
    ?assertNotMatch(undefined, get_timing_value(request_end)),
    %% check timings
    ?assert(?VTB(request_start, request_end, 1000000, 1200000)),
    ?assert(?VTB(headers_start, headers_end, 1, 100)),
    ?assert(?VTB(body_start, body_end, 1, 100)),
    ?assert(?VTB(user_start, user_end, 1000000, 1200000)),
    ?assert(?VTB(send_start, send_end, 1, 2000)).

compress(Encoding, Length) ->
    Response = hackney:get("http://localhost:3001/compressed",
                           [{<<"Accept-Encoding">>, Encoding}]),
    ?assertMatch(200, status(Response)),
    ?assertHeadersEqual([{<<"Content-Encoding">>, Encoding},
                  {<<"connection">>, <<"Keep-Alive">>},
                  {<<"content-length">>, Length}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 uncompress(Encoding, body(Response))).

uncompress(<<"gzip">>,    Data) -> zlib:gunzip(Data);
uncompress(<<"deflate">>, Data) -> zlib:uncompress(Data).

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
    %%?LOG_INFO("~p~n", [Part]),
    gen_tcp:send(Socket, Part),
    timer:sleep(1),
    send(Socket, Rest, ChunkSize).


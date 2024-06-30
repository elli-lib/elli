-module(elli_http_SUITE).
-include_lib("stdlib/include/assert.hrl").

-compile([export_all, nowarn_export_all]).

%
% Configuration.

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        not lists:member(Fun, [module_info,
                               chunk_loop_wrapper,
                               status, body, headers])
    ].

%
% Tests.

chunk_loop(_Config) ->
    Here    = self(),
    Pid     = spawn_link(chunk_loop_wrapper(Here)),
    Pid     ! {tcp_closed, some_socket},
    Message = receive_message(),
    ?assertMatch({error, client_closed}, Message).

get_body(_Config) ->
    Socket   = undefined,
    Headers  = [{<<"Content-Length">>, <<" 42 ">>}],
    Buffer   = binary:copy(<<".">>, 42),
    Opts     = [],
    Callback = {no_mod, []},
    ?assertMatch({Buffer, <<>>},
                 elli_http:get_body(Socket, Headers, Buffer, Opts, Callback)).

%
% Private.

chunk_loop_wrapper(Here) ->
    fun () ->
        Result = elli_http:chunk_loop({some_type, some_socket}),
        Here   ! Result,
        ok
    end.

receive_message() ->
    receive
        X -> X
    after
        1 -> fail
    end.

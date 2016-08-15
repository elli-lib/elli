-module(elli_http_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Unit tests

chunk_loop_test_() ->
  fun () ->
      Here    = self(),
      Pid     = spawn_link(chunk_loop_wrapper(Here)),
      Pid     ! {tcp_closed, some_socket},
      Message = receive
                  X -> X
                after
                  1 -> fail
                end,
      ?assertEqual({error, client_closed}, Message)
  end.

chunk_loop_wrapper(Here) ->
  fun() ->
      Result = elli_http:chunk_loop({some_type, some_socket}),
      Here   ! Result,
      ok
  end.

-module(elli_metrics_middleware).
-export([handle/2, handle_event/3]).
-behaviour(elli_handler).


%%
%% ELLI
%%

handle(_Req, _Args) ->
    ignore.


%%
%% ELLI EVENT CALLBACKS
%%

handle_event(request_complete, [_Req,_C,_Hs,_B, {Timings, Sizes}], _) ->
    ets:insert(elli_stat_table, {timings, Timings}),
    ets:insert(elli_stat_table, {sizes, Sizes});
handle_event(chunk_complete, [_Req,_C,_Hs,_B, {Timings, Sizes}], _) ->
    ets:insert(elli_stat_table, {timings, Timings}),
    ets:insert(elli_stat_table, {sizes, Sizes});
handle_event(_Event, _Data, _Args) ->
    ok.

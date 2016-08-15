-module(elli_handler).
-include("elli.hrl").

-export_type([result/0, callback_args/0]).

-type result() :: ignore | {elli:response_code() | ok, [tuple()], binary()}.

-callback handle(Req :: elli:req(), callback_args()) -> result().

-callback handle_event(Event, Args, Config) -> ok when
    Event  :: elli_event(),
    Args   :: [term()],
    Config :: [tuple()].

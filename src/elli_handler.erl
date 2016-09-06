-module(elli_handler).

-export_type([callback/0,callback_mod/0,callback_args/0,event/0,result/0]).

%% @type callback(). A tuple of a {@type callback_mod()} and {@type
%% callback_args()}.
-type callback() :: {callback_mod(), callback_args()}.

%% @type callback_mod(). A callback module.
-type callback_mod()  :: module().

%% @type callback_args(). Arguments to pass to a {@type callback_mod()}.
-type callback_args() :: list().

%% @type event(). Fired throughout processing a request.
%% See {@link elli_example_callback:handle_event/3} for descriptions.
-type event() :: elli_startup
               | bad_request    | file_error
               | chunk_complete | request_complete
               | request_throw  | request_error       | request_exit
               | request_closed | request_parse_error
               | client_closed  | client_timeout
               | invalid_return.

-type result() :: {elli:response_code() | ok, elli:body()}
                | {elli:response_code() | ok, elli:headers(), elli:body()}
                | ignore.

-callback handle(Req :: elli:req(), callback_args()) -> result().

-callback handle_event(Event, Args, Config) -> ok when
    Event  :: event(),
    Args   :: callback_args(),
    Config :: [tuple()].

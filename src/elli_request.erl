-module(elli_request).
-include("elli.hrl").
-include("elli_util.hrl").

-export([send_chunk/2
        , async_send_chunk/2
        , chunk_ref/1
        , close_chunk/1
        , path/1
        , raw_path/1
        , query_str/1
        , get_header/2
        , get_header/3
        , get_arg_decoded/2
        , get_arg_decoded/3
        , get_arg/2
        , get_arg/3
        , get_args/1
        , get_args_decoded/1
        , post_arg/2
        , post_arg/3
        , post_arg_decoded/2
        , post_arg_decoded/3
        , post_args/1
        , post_args_decoded/1
        , body_qs/1
        , original_headers/1
        , headers/1
        , peer/1
        , method/1
        , body/1
        , scheme/1
        , host/1
        , port/1
        , get_range/1
        , to_proplist/1
        , is_request/1
        , uri_decode/1
        ]).

-export_type([http_range/0]).

-type http_range() :: {First::non_neg_integer(), Last::non_neg_integer()}
                    | {offset, Offset::non_neg_integer()}
                    | {suffix, Length::pos_integer()}.

-elvis([{elvis_style, god_modules, disable}]).

%%
%% Helpers for working with a `#req{}'
%%


%% @doc Return `path' split into binary parts.
path(#req{path = Path})          -> Path.
%% @doc Return the `raw_path', i.e. not split or parsed for query params.
raw_path(#req{raw_path = Path})  -> Path.
%% @doc Return the `headers' that have had `string:casefold/1' run on each key.
headers(#req{headers = Headers}) -> Headers.
%% @doc Return the original `headers'.
original_headers(#req{original_headers = Headers}) -> Headers.
%% @doc Return the `method'.
method(#req{method = Method})    -> Method.
%% @doc Return the `body'.
body(#req{body = Body})          -> Body.
%% @doc Return the `scheme'.
scheme(#req{scheme = Scheme})    -> Scheme.
%% @doc Return the `host'.
host(#req{host = Host})          -> Host.
%% @doc Return the `port'.
port(#req{port = Port})          -> Port.

peer(#req{socket = Socket} = _Req) ->
    case elli_tcp:peername(Socket) of
        {ok, {Address, _}} ->
            list_to_binary(inet_parse:ntoa(Address));
        {error, _} ->
            undefined
    end.

get_header(Key, #req{headers = Headers}) ->
    CaseFoldedKey = string:casefold(Key),
    proplists:get_value(CaseFoldedKey, Headers).

get_header(Key, #req{headers = Headers}, Default) ->
    CaseFoldedKey = string:casefold(Key),
    proplists:get_value(CaseFoldedKey, Headers, Default).


%% The same as `get_arg(Key, Req, undefined)'
get_arg(Key, #req{} = Req) ->
    get_arg(Key, Req, undefined).

%% The same as `proplists:get_value(Key, Args, Default)'
get_arg(Key, #req{args = Args}, Default) ->
    proplists:get_value(Key, Args, Default).

%% The same as `get_arg_decoded(Key, Req, undefined)'
get_arg_decoded(Key, #req{} = Req) ->
    get_arg_decoded(Key, Req, undefined).

get_arg_decoded(Key, #req{args = Args}, Default) ->
    case proplists:get_value(Key, Args) of
        undefined    -> Default;
        true         -> true;
        EncodedValue ->
            uri_decode(EncodedValue)
    end.

%% @doc Parse `application/x-www-form-urlencoded' body into a proplist.
body_qs(#req{body = <<>>}) -> [];
body_qs(#req{body = Body} = Req) ->
    case get_header(<<"Content-Type">>, Req) of
        <<"application/x-www-form-urlencoded">> ->
            elli_http:split_args(Body);
        <<"application/x-www-form-urlencoded;", _/binary>> -> % ; charset=...
            elli_http:split_args(Body);
        _ ->
            erlang:error(badarg)
    end.

%% The same as `post_arg(Key, Req, undefined)'
post_arg(Key, #req{} = Req) ->
    post_arg(Key, Req, undefined).

post_arg(Key, #req{} = Req, Default) ->
    proplists:get_value(Key, body_qs(Req), Default).

%% The same as `post_arg_decoded(Key, Req, undefined)'
post_arg_decoded(Key, #req{} = Req) ->
    post_arg_decoded(Key, Req, undefined).

post_arg_decoded(Key, #req{} = Req, Default) ->
    case proplists:get_value(Key, body_qs(Req)) of
        undefined    -> Default;
        true         -> true;
        EncodedValue ->
            uri_decode(EncodedValue)
    end.


%% @doc Return a proplist of keys and values of the original query string.
%% Both keys and values in the returned proplists will be binaries or the atom
%% `true' in case no value was supplied for the query value.
-spec get_args(elli:req())  -> QueryArgs :: proplists:proplist().
get_args(#req{args = Args}) -> Args.

get_args_decoded(#req{args = Args}) ->
    lists:map(fun ({K, true}) ->
                      {K, true};
                  ({K, V}) ->
                      {K, uri_decode(V)}
              end, Args).


post_args(#req{} = Req) ->
    body_qs(Req).

post_args_decoded(#req{} = Req) ->
    lists:map(fun ({K, true}) ->
                      {K, true};
                  ({K, V}) ->
                      {K, uri_decode(V)}
              end, body_qs(Req)).

%% @doc Calculate the query string associated with a given `Request'
%% as a binary.
-spec query_str(elli:req()) -> QueryStr :: binary().
query_str(#req{raw_path = Path}) ->
    case binary:split(Path, [<<"?">>]) of
        [_, Qs] -> Qs;
        [_]     -> <<>>
    end.


%% @doc Parse the `Range' header from the request.
%% The result is either a `[http_range()]' or the atom `parse_error'.
%% Use `elli_util:normalize_range/2' to get a validated, normalized range.
-spec get_range(elli:req()) -> [http_range()] | parse_error.
get_range(#req{headers = Headers})  ->
    case proplists:get_value(<<"range">>, Headers) of
        <<"bytes=", RangeSetBin/binary>> ->
            parse_range_set(RangeSetBin);
        _ -> []
    end.


-spec parse_range_set(Bin::binary()) -> [http_range()] | parse_error.
parse_range_set(<<ByteRangeSet/binary>>) ->
    RangeBins = binary:split(ByteRangeSet, <<",">>, [global]),
    Parsed    = [parse_range(remove_whitespace(RangeBin))
                 || RangeBin <- RangeBins],
    case lists:member(parse_error, Parsed) of
        true  -> parse_error;
        false -> Parsed
    end.

-spec parse_range(Bin::binary()) -> http_range() | parse_error.
parse_range(<<$-, SuffixBin/binary>>) ->
    %% suffix-byte-range
    try {suffix, binary_to_integer(SuffixBin)}
    catch
        error:badarg -> parse_error
    end;
parse_range(<<ByteRange/binary>>) ->
    case binary:split(ByteRange, <<"-">>) of
        %% byte-range without last-byte-pos
        [FirstBytePosBin, <<>>] ->
            try {offset, binary_to_integer(FirstBytePosBin)}
            catch
                error:badarg -> parse_error
            end;
        %% full byte-range
        [FirstBytePosBin, LastBytePosBin] ->
            try {bytes,
                 binary_to_integer(FirstBytePosBin),
                 binary_to_integer(LastBytePosBin)}
            catch
                error:badarg -> parse_error
            end;
        _ -> parse_error
    end.

-spec remove_whitespace(binary()) -> binary().
remove_whitespace(Bin) ->
    binary:replace(Bin, <<" ">>, <<>>, [global]).

%% @doc Serialize the `Req'uest record to a proplist.
%% Useful for logging.
to_proplist(#req{} = Req) ->
    lists:zip(record_info(fields, req), tl(tuple_to_list(Req))).



%% @doc Return a reference that can be used to send chunks to the client.
%% If the protocol does not support it, return `{error, not_supported}'.
chunk_ref(#req{version = {1, 1}} = Req) ->
    Req#req.pid;
chunk_ref(#req{}) ->
    {error, not_supported}.


%% @doc Explicitly close the chunked connection.
%% Return `{error, closed}' if the client already closed the connection.
%% The same as `send_chunk(Ref, close)'
close_chunk(Ref) ->
    send_chunk(Ref, close).

%% @doc Send a chunk asynchronously.
async_send_chunk(Ref, Data) ->
    Ref ! {chunk, Data}.

%% @doc Send a chunk synchronously.
%% If the referenced process is dead, return early with `{error, closed}',
%% instead of timing out.
send_chunk(Ref, Data) ->
    ?IF(is_ref_alive(Ref),
        send_chunk(Ref, Data, 5000),
        {error, closed}).

send_chunk(Ref, Data, Timeout) ->
    Ref ! {chunk, Data, self()},
    receive
        {Ref, ok} ->
            ok;
        {Ref, {error, Reason}} ->
            {error, Reason}
    after Timeout ->
            {error, timeout}
    end.

is_ref_alive(Ref) ->
    ?IF(node(Ref) =:= node(),
        is_process_alive(Ref),
        rpc:call(node(Ref), erlang, is_process_alive, [Ref])).

is_request(#req{}) -> true;
is_request(_)      -> false.

uri_decode(Bin) ->
    case binary:match(Bin, [<<"+">>, <<"%">>]) of
        nomatch -> Bin;
        {Pos, _} ->
            <<Prefix:Pos/binary, Rest/binary>> = Bin,
            uri_decode(Rest, Prefix)
    end.

uri_decode(<<>>, Acc) -> Acc;
uri_decode(<<$+, Rest/binary>>, Acc) ->
    uri_decode(Rest, <<Acc/binary, $\s>>);
uri_decode(<<$%, H, L, Rest/binary>>, Acc) ->
    uri_decode(Rest, <<Acc/binary, (hex_to_int(H)):4, (hex_to_int(L)):4>>);
uri_decode(<<C, Rest/binary>>, Acc) ->
    uri_decode(Rest, <<Acc/binary, C>>).

-compile({inline, [hex_to_int/1]}).

hex_to_int(X) when X >= $0, X =< $9 -> X - $0;
hex_to_int(X) when X >= $a, X =< $f -> X - ($a-10);
hex_to_int(X) when X >= $A, X =< $F -> X - ($A-10).

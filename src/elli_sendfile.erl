%% Taken from https://github.com/ninenines/ranch/pull/41/files,
%% with permission from fishcakez

-module(elli_sendfile).

-export([sendfile/6]).

-type socket() :: any().
-type sendfile_opts() :: [{chunk_size, non_neg_integer()}].

%% @doc Send part of a file on a socket.
%%
%% A fallback for transports that don't have a native sendfile implementation.
%% Note that the ordering of arguments is different from file:sendfile/5 and
%% that this function accepts either a raw file or a file name.
%%
%% @see file:sendfile/5
-spec sendfile(module(), socket(), file:filename_all() | file:fd(),
        non_neg_integer(), non_neg_integer(), sendfile_opts())
    -> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Transport, Socket, Filename, Offset, Bytes, Opts)
        when is_list(Filename) orelse is_atom(Filename)
        orelse is_binary(Filename) ->
    ChunkSize = chunk_size(Opts),
    case file:open(Filename, [read, raw, binary]) of
        {ok, RawFile} ->
            _ = case Offset of
                0 ->
                    ok;
                _ ->
                    {ok, _} = file:position(RawFile, {bof, Offset})
            end,
            try
                sendfile_loop(Transport, Socket, RawFile, Bytes, 0, ChunkSize)
            after
                ok = file:close(RawFile)
            end;
        {error, _Reason} = Error ->
            Error
    end;
sendfile(Transport, Socket, RawFile, Offset, Bytes, Opts) ->
    ChunkSize = chunk_size(Opts),
    Initial2 = case file:position(RawFile, {cur, 0}) of
        {ok, Offset} ->
            Offset;
        {ok, Initial} ->
            {ok, _} = file:position(RawFile, {bof, Offset}),
            Initial
        end,
    case sendfile_loop(Transport, Socket, RawFile, Bytes, 0, ChunkSize) of
        {ok, _Sent} = Result ->
            {ok, _} = file:position(RawFile, {bof, Initial2}),
            Result;
        {error, _Reason} = Error ->
            Error
    end.

-spec chunk_size(sendfile_opts()) -> pos_integer().
chunk_size(Opts) ->
    case lists:keyfind(chunk_size, 1, Opts) of
        {chunk_size, ChunkSize}
                when is_integer(ChunkSize) andalso ChunkSize > 0 ->
            ChunkSize;
        {chunk_size, 0} ->
            16#1FFF;
        false ->
            16#1FFF
    end.

-spec sendfile_loop(module(), socket(), file:fd(), non_neg_integer(),
        non_neg_integer(), pos_integer())
    -> {ok, non_neg_integer()} | {error, term()}.
sendfile_loop(_Transport, _Socket, _RawFile, Sent, Sent, _ChunkSize)
        when Sent =/= 0 ->
    %% All requested data has been read and sent, return number of bytes sent.
    {ok, Sent};
sendfile_loop(Transport, Socket, RawFile, Bytes, Sent, ChunkSize) ->
    ReadSize = read_size(Bytes, Sent, ChunkSize),
    case file:read(RawFile, ReadSize) of
        {ok, IoData} ->
            case Transport:send(Socket, IoData) of
                ok ->
                    Sent2 = iolist_size(IoData) + Sent,
                    sendfile_loop(Transport, Socket, RawFile, Bytes, Sent2,
                        ChunkSize);
                {error, _Reason} = Error ->
                    Error
            end;
        eof ->
            {ok, Sent};
        {error, _Reason} = Error ->
            Error
    end.

-spec read_size(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    non_neg_integer().
read_size(0, _Sent, ChunkSize) ->
    ChunkSize;
read_size(Bytes, Sent, ChunkSize) ->
    min(Bytes - Sent, ChunkSize).

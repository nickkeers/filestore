-module(es3).

%% API
-export([
    write/2,
    read/1,
    delete/1,
    chunk_binary/1
]).

-define(TIMEOUT, 10000).

-spec write(Name, Data) -> Res when
    Name :: iodata(),
    Data :: binary(),
    Res :: ok | {error, Reason :: any()}.
write(Name, Data) ->
    % This gives us a list of chunks in reverse order, so we can be crafty and label our chunks in descending order
    % to save us reversing a potentially large list
    Chunked = chunk_binary(Data),
    ChunkListLength = length(Chunked),
    Peers = nodes(),
    RepeatedNodes = repeat(Peers, length(Chunked)),

    %% Gives us a list of [{Idx, Chunk, Checksum}..{IdxN, ChunkN, ChecksumN}]
    IndexedChunks = lists:zip3(lists:seq(ChunkListLength, 1, -1), Chunked, RepeatedNodes),

    lists:foreach(fun({Index, Chunk, Node}) ->
                      Checksum = es3_chunk:checksum(Chunk),
                      rpc:call(Node, store, write, [Name, Index, Chunk, Checksum])
                  end, IndexedChunks),
    ok.

repeat(L, N) when length(L) < N ->
    repeat(L, N-length(L), L);
repeat(L, _N) ->
    L.

repeat(_, 0, Acc) ->
    Acc;
repeat([H | T], N, Acc) ->
    repeat(T ++ [H], N-1, [H|Acc]).

-spec read(Name) -> Object when
    Name :: iodata(),
    Object :: binary() | {error, Reason :: any()}.
read(Name) ->
    Self = self(),
    {ok, _Pid} = file_reader_sup:start_reader(Name, Self),
    receive
        {results, Results} -> Results;
        {error, Rsn} -> {error, Rsn}
    end.

-spec delete(Name) -> Res when
    Name :: iodata(),
    Res :: ok | {error, Reason :: any()}.
delete(Name) ->
    store:delete(Name).

-spec chunk_binary(binary()) -> [binary()].
chunk_binary(Data) ->
    ChunkSize = application:get_env(filestore, chunk_size, 1000000),
    chunk_binary(Data, ChunkSize, []).

chunk_binary(<<>>, _, Acc) ->
    Acc;
chunk_binary(Data, ChunkSize, Acc) ->
    case Data of
        <<Chunk:ChunkSize/binary, Rest/binary>> when byte_size(Data) > ChunkSize ->
            chunk_binary(Rest, ChunkSize, [binary:copy(Chunk) | Acc]);
        RemainingChunk ->
            chunk_binary(<<>>, ChunkSize, [binary:copy(RemainingChunk) | Acc])
    end.
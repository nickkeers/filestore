-module(file_reader).

%% API
-export([
    start_link/2,
    reader/3,
    read_node_chunks/3
]).

start_link(Filename, Parent) ->
    Self = self(),
    proc_lib:start_link(?MODULE, reader, [Filename, Parent, Self]).

reader(Filename, Parent, Self) ->
    ThisPid = self(),
    proc_lib:init_ack(Self, {ok, ThisPid}),
    Peers = nodes(),

    % Gives list of [{Node, [Key = {Filename, ChunkIndex}]]
    Metadata = lists:map(fun(Node) ->
        Entries =
            case gen_server:call({store, Node}, {entries, Filename}) of
                {error, _Reason} ->
                    io:format("Couldn't fetch chunk on node ~p for ~p~n", [Node, Filename]),
                    {error, {no_chunk, Node, Filename}};
                Entries2 ->
                    Entries2
            end,
        {Node, Entries}
                         end, Peers),

    HowMany = lists:foldl(fun({_Node, Entries}, Acc) -> Acc + length(Entries) end, 0, Metadata),

    [spawn(?MODULE, read_node_chunks, [ThisPid, Node, Key]) || {Node, Key} <- Metadata],

    Results = wait_for_results(0, HowMany, []),
    Parent ! reassemble(Results).

-spec read_node_chunks(pid(), node(), [{iodata(), integer()}]) -> ok.
read_node_chunks(Collector, _, {error, Rsn} = ER) ->
    io:format("Error reading chunk: ~p~n", [Rsn]),
    Collector ! ER;
read_node_chunks(CollectorPid, Node, Keys) ->
    lists:foreach(fun(Key) ->
        Chunk = gen_server:call({store, Node}, {read_chunk, Key}),
        CollectorPid ! {chunk, {Key, Chunk}}
    end, Keys).

wait_for_results(Collected, Total, Acc) when Collected == Total ->
    Acc;
wait_for_results(Collected, Total, Acc) ->
    receive
        {chunk, {_, {error, _}}} ->
            {error, missing_chunk};
        {chunk, {Key, Chunk}} ->
            wait_for_results(Collected +1, Total, [{Key, Chunk} | Acc])
    after 5000 ->
        {error, timeout}
    end.

reassemble({error, Reason}) ->
    {error, Reason};
reassemble(Chunks) ->
    Sorted = lists:sort(fun({{_Filename, Index}, _Chunk}, {{_Filename2, Index2}, _Chunk2}) ->
        Index =< Index2
                        end, Chunks),
    io:format("Sorted chunks: ~p~n", [Sorted]),
    Res = lists:foldl(fun({_, Chunk}, Acc) ->
        <<Acc/binary, Chunk/binary>>
                end, <<>>, Sorted),
    {results, Res}.
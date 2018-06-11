-module(file_reader).

%% API
-export([
    start_link/2,
    reader/3,
    read_node_chunks/3
]).

-spec start_link(iodata(), pid()) -> {'ok', pid()} | {'error', term()}.
start_link(Filename, Parent) ->
    Self = self(),
    proc_lib:start_link(?MODULE, reader, [Filename, Parent, Self]).

-spec reader(iodata(), pid(), pid()) -> ok.
reader(Filename, Parent, Self) ->
    ThisPid = self(),
    proc_lib:init_ack(Self, {ok, ThisPid}),

    Metadata = store:all_entries_for_filename(Filename),

    HowMany = lists:foldl(fun({_Node, Entries}, Acc) -> Acc + length(Entries) end, 0, Metadata),

    [spawn(?MODULE, read_node_chunks, [ThisPid, Node, Key]) || {Node, Key} <- Metadata],

    Results = wait_for_results(0, HowMany, []),
    Parent ! reassemble(Results),
    ok.

-spec read_node_chunks(pid(), node(), [{iodata(), non_neg_integer()}]) -> ok.
read_node_chunks(Collector, _, {error, Rsn} = ER) ->
    io:format("Error reading chunk: ~p~n", [Rsn]),
    Collector ! ER;
read_node_chunks(CollectorPid, Node, Keys) ->
    lists:foreach(fun(Key) ->
        Chunk = gen_server:call({store, Node}, {read_chunk, Key}),
        CollectorPid ! {chunk, {Key, Chunk}}
    end, Keys).

-spec wait_for_results(non_neg_integer(),number(),[{{iodata(), non_neg_integer()}, binary()}]) -> [{iodata(), non_neg_integer()}] | {'error','missing_chunk' | 'timeout'}.
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

-spec reassemble([{binary() | maybe_improper_list(iodata(),binary() | []),non_neg_integer()}] | {'error','missing_chunk' | 'timeout'}) -> {'error','missing_chunk' | 'timeout'} | {'results',<<>>}.
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
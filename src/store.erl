-module(store).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    write/4,
    write_to_peers/4,
    get_all_chunk_entries/0,
    read/1,
    read_chunk/1,
    delete/1,
    entries_for_filename/1,
    reset/0
]).

-define(SERVER, ?MODULE).
-define(MATCH_SPEC(Name), [{{{'$1', '$2'}, '_'}, [{'==', '$1', Name}], [{{'$1', '$2'}}]}]).
-define(DELETE_SPEC(Name), [{{{'$1', '_'}, '_'}, [{'==', '$1', Name}], [true]}]).

-record(state, {
    meta_tab :: dets:tab_name(),
    chunk_tab :: dets:tab_name()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec write_to_peers(Filename :: iodata(), ChunkIndex :: integer(), Chunk :: binary(), Checksum :: integer()) -> ok.
write_to_peers(Filename, ChunkIndex, Chunk, Checksum) ->
    gen_server:multi_call(nodes(), store, {write, ChunkIndex, Filename, Chunk, Checksum}).


%% @doc
%% Return a list of unique chunks that the current node knows about, we can use this to rebuild state of other nodes,
%% perform naive node repairs later if we need to - or present data via the REST API if needed
%% @end
-spec get_all_chunk_entries() -> sets:new() | {error, term()}.
get_all_chunk_entries() ->
    dets:foldl(fun({{Filename, Index}, _Checksum, _Node}, Acc) ->
        gb_sets:add({Filename, Index}, Acc)
               end, gb_sets:new(), store).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    erlang:send_after(5000, self(), connect),
    case dets:open_file(store, []) of
        {ok, DHandle} ->
            case dets:open_file(chunks, []) of
                {ok, ChunkHandle} ->
                    {ok, #state{meta_tab = DHandle, chunk_tab = ChunkHandle}};
                {error, _} = E ->
                    io:format("Couldn't open chunks table, error: ~p~n", [E]),
                    exit(1)
            end;
        {error, _Reason} = E ->
            io:format("Unable to open database file: ~p~n", [E]),
            ignore
    end.

handle_call({write, Filename, ChunkIndex, Chunk, Checksum}, _From, State = #state{meta_tab = Table, chunk_tab = ChunkTab}) ->
    ok = dets:insert(Table, {{Filename, ChunkIndex}, Checksum}),
    ok = dets:insert(ChunkTab, {{Filename, ChunkIndex}, Chunk}),
    {reply, ok, State};
%% Read a metdata entry
handle_call({read, Filename, ChunkIndex}, _From, State = #state{meta_tab = Table}) ->
    Reply =
        case dets:lookup({Filename, ChunkIndex}, Table) of
            [{{Filename, ChunkIndex}, _Checksum} = Row] ->
                Row;
            _ ->
                {error, no_such_entry}
        end,
    {reply, Reply, State};
handle_call({read_chunk, Key}, _From, State = #state{chunk_tab = Table}) ->
    Result = case dets:lookup(Table, Key) of
                 [{_, Chunk}] -> Chunk;
                 {error, _} = ER-> ER
             end,
    {reply, Result, State};
handle_call({entries, Filename}, _From, State = #state{meta_tab = Table}) ->
    Reply =
        case dets:select(Table, ?MATCH_SPEC(Filename)) of
            {error, _Reason} = Error ->
                Error;
            Selection ->
                Selection
        end,
    {reply, Reply, State};
handle_call({delete, Name}, _From, State = #state{meta_tab = Table, chunk_tab = ChunkTab}) ->
    _ = dets:select_delete(Table, ?DELETE_SPEC(Name)),
    _ = dets:select_delete(ChunkTab, ?DELETE_SPEC(Name)),
    {reply, ok, State};
handle_call(reset, _From, State = #state{meta_tab = MetaTab, chunk_tab = ChunkTab}) ->
    dets:delete_all_objects(MetaTab),
    dets:delete_all_objects(ChunkTab),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    {ok, NumNodes} = application:get_env(filestore, num_nodes),
    [ net_adm:ping(node_name_for(N)) || N <- lists:seq(1, NumNodes) ],
    erlang:send_after(5000, self(), connect),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{meta_tab = DetsTab}) ->
    dets:close(DetsTab),
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec node_name_for(N :: integer()) -> atom().
node_name_for(N) ->
    {ok, Host} = inet:gethostname(),
    list_to_atom("es3_" ++ integer_to_list(N) ++ "@" ++ Host).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-type row() :: {{iodata(), integer()}, binary(), integer(), node()}.

%% @doc
%% Write a file chunk to the in-memory store
%% @end
-spec write(Filename :: iodata(), ChunkIndex :: integer(), Chunk :: binary(), Checksum :: integer()) -> ok.
write(Filename, ChunkIndex, Chunk, Checksum) ->
    gen_server:call(?MODULE, {write, Filename, ChunkIndex, Chunk, Checksum}).

%% @doc
%% Read a single entry for a filename and the given chunk index
%% @end
-spec read({iodata(), integer()}) -> row() | {error, no_such_entry}.
read({Filename, ChunkIndex}) ->
    gen_server:call(?MODULE, {read, Filename, ChunkIndex}).

-spec read_chunk({iodata(), integer()}) -> row() | {error, term()}.
read_chunk(Key) ->
    gen_server:call(?MODULE, {read_chunk, Key}).

%% @doc
%% Return all the entries in the store for the given filename
%% @end
-spec entries_for_filename(iodata()) -> [row()] | {error, no_entries}.
entries_for_filename(Filename) ->
    gen_server:call(?MODULE, {entries, Filename}).

-spec delete(Key :: iodata()) -> ok | {error, term()}.
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, reset).
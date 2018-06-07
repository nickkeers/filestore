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
    write_to_peers/5,
    get_all_chunk_entries/0,
    read/1,
    delete/1,
    entries_for_filename/1
]).

-define(SERVER, ?MODULE).

-record(state, {
    table :: dets:tab_name()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec write_to_peers(Filename :: iodata(), ChunkIndex :: integer(), Chunk :: binary(), Checksum :: integer(), Location :: node()) -> ok.
write_to_peers(Filename, ChunkIndex, Chunk, Checksum, Location) ->
    gen_server:multi_call(es3_master_server:peers(), meta_store, {write, ChunkIndex, Filename, Chunk, Checksum, Location}).


%% @doc
%% Return a list of unique chunks that the current node knows about, we can use this to rebuild state of other nodes,
%% perform naive node repairs later if we need to - or present data via the REST API if needed
%% @end
-spec get_all_chunk_entries() -> sets:new() | {error, term()}.
get_all_chunk_entries() ->
    dets:foldl(fun({{Filename, Index}, _Chunk, _Checksum, _Node}, Acc) ->
        gb_sets:add({Filename, Index}, Acc)
               end, gb_sets:new(), metadata).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case dets:open_file(store, []) of
        {ok, DHandle} ->
            {ok, #state{table = DHandle}};
        {error, _Reason} = E ->
            io:format("Unable to open database file: ~p~n", [E]),
            ignore
    end.

handle_call({write, Filename, ChunkIndex, Chunk, Checksum, Node}, _From, State = #state{table = Table}) ->
    ok = dets:insert(Table, {{Filename, ChunkIndex}, Chunk, Checksum, Node}),
    {reply, ok, State};
handle_call({read, Filename, ChunkIndex}, _From, State = #state{table = Table}) ->
    Reply =
        case dets:lookup({Filename, ChunkIndex}, Table) of
            [{{Filename, ChunkIndex}, _Checksum, _Node} = Row] ->
                Row;
            _ ->
                {error, no_such_entry}
        end,
    {reply, Reply, State};
handle_call({entries, Filename}, _From, State = #state{table = Table}) ->
    Reply =
        case dets:select(Table, [{{{'$1', '_'}, '_', '_', '_'}, [{'==', '$1', Filename}], ['$_']}]) of
            {error, _Reason} = Error ->
                Error;
            Selection ->
                Selection
        end,
    {reply, Reply, State};
handle_call({delete, Key}, _From, State = #state{table = Table}) ->
    Reply = dets:delete(Table, Key),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{table = DetsTab}) ->
    dets:close(DetsTab),
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-type row() :: {{iodata(), integer()}, binary(), integer(), node()}.

%% @doc
%% Write a file chunk to the in-memory store
%% @end
-spec write(Filename :: iodata(), ChunkIndex :: integer(), Chunk :: binary(), Checksum :: integer()) -> ok.
write(Filename, ChunkIndex, Chunk, Checksum) ->
    gen_server:call(?MODULE, {write, Filename, ChunkIndex, Chunk, Checksum, node()}).

%% @doc
%% Read a single entry for a filename and the given chunk index
%% @end
-spec read({iodata(), integer()}) -> row() | {error, no_such_entry}.
read({Filename, ChunkIndex}) ->
    gen_server:call(?MODULE, {read, Filename, ChunkIndex}).

%% @doc
%% Return all the entries in the store for the given filename
%% @end
-spec entries_for_filename(iodata()) -> [row()] | {error, no_entries}.
entries_for_filename(Filename) ->
    gen_server:call(?MODULE, {entries, Filename}).

-spec delete(Key :: {iodata(), integer()}) -> ok | {error, term()}.
delete(Key = {_Filename, _ChunkIndex}) ->
    gen_server:call(?MODULE, {delete, Key}).
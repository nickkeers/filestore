-module(meta_store).

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
  write/2,
  write_to_peers/4,
  get_all_chunk_entries/0
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

-spec write_to_peers(Filename :: string(), ChunkName :: es3_chunk:chunk_name(), Checksum :: integer(), Location :: node()) -> ok.
write_to_peers(Filename, ChunkName, Checksum, Location) ->
  gen_server:multi_call(es3_master_server:peers(), meta_store, {write, ChunkName, Filename, Checksum, Location}).


%% @doc
%% Return a list of unique chunks that the current node knows about, we can use this to rebuild state of other nodes,
%% perform naive node repairs later if we need to - or present data via the REST API if needed
%% @end
-spec get_all_chunk_entries() -> sets:new() | {error, term()}.
get_all_chunk_entries() ->
  dets:foldl(fun({Filename, Index, _Checksum}, Acc) ->
                gb_sets:add({Filename, Index}, Acc)
             end, gb_sets:new(), metadata).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  case dets:open_file(metadata, []) of
    {ok, DHandle} ->
      {ok, #state{table = DHandle}};
    {error, _Reason} = E ->
      io:format("Unable to open database file: ~p~n", [E]),
      ignore
  end.

handle_call({write, ChunkKey, Filename, Checksum, Node}, _From, State = #state{table = Table}) ->
  ok = dets:insert(Table, {ChunkKey, Filename, Checksum, Node}),
  {reply, ok, State};
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

-spec write(binary(), list()) -> ok.
write(ChunkKey, Filename) ->
  gen_server:call(?MODULE, {write, ChunkKey, Filename, node()}).
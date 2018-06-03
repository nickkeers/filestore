%% @doc
%% Simple gossip protocol to update metadata
%% @end
-module(es3_master_server).

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
  peers/0
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MULTICAST_INTERVAL, 30000).

-record(state, {
  peers = []
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_to_all(Pid, Msg) ->
  gen_server:cast(?SERVER, {to_all, Pid, Msg}).

peers() ->
  gen_server:call(?SERVER, peers).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Peers = nodes() -- [node()],
  {ok, #state{peers = Peers}}.

handle_call(peers, _From, State) ->
  {reply, State#state.peers, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({to_all, Regname, Msg}, State = #state{peers = Peers}) ->
  handle_send_to_all(Regname, Msg, Peers),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_send_to_all(Regname, Msg, Peers) ->
  [ {Node, Regname} ! Msg || Node <- Peers ].
-module(pool).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-export([
  checkout/1,
  checkin/2
]).

-define(SERVER, ?MODULE).

-record(state, {
  waiting = [],
  tab
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(Name :: atom(), Opts :: proplists:proplist()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [], Opts).

%% @doc
%% take a worker from the pool - blocking call
%% @end
-spec checkout(atom()) -> pid() | {error, term()}.
checkout(Name) ->
  try
    gen_server:call(Name, checkout, infinity)
  catch
      E:R  ->
        {error, R}
  end.

-spec checkin(Name :: atom(), Pid :: pid()) -> ok | {error, term()}.
checkin(Name, Pid) ->
  try
    gen_server:call(Name, {checkin, Pid}, infinity)
  catch
    E:R ->
      {error, R}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

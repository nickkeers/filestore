-module(conn_server).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
  get_connection/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  start_port,
  max_conns,
  connections = [] % List of listening sockets for this node
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_connection(Filename :: iodata()) -> {ok, integer(), pid()}.
get_connection(FileName) ->
  gen_server:call(?SERVER, {get_connection, FileName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  process_flag(trap_exit, true),
  {ok, PortStart} = application:get_env(filestore, port_start),
  MaxConns  = application:get_env(filestore, max_conns, 10),

  Connections = lists:map(fun(I) ->
    case conn_sup:start_connection(PortStart + I) of
      {ok, Pid} ->
        MRef = erlang:monitor(process, Pid),
        {PortStart + I, MRef, Pid}
    end
  end, lists:seq(0, MaxConns)),

  {ok, #state{
    start_port = PortStart,
    max_conns = MaxConns,
    connections = Connections
  }}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({get_connection, Filename}, _From, State = #state{connections = Conns}) ->
  NumConns = length(Conns),
  Place = erlang:phash2(Filename, NumConns),
  {Port, _, _Pid} = lists:nth(Place, Conns),
  {reply, {ok, Port}, State};
handle_call(get_random_ip_port, _From, State = #state{connections = Conns}) ->
  {Port, _, _} = lists:nth(rand:uniform(length(Conns)), Conns),
  {reply, Port, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{connections = Connections}) ->
  NewConns = lists:keydelete(Pid, 3, Connections),
  {Port, _, _} = lists:keyfind(Pid, 3, Connections),
  erlang:send_after(10000, self(), {new_conn, Port, 10000}), % give time for underlying socket to close
  {noreply, State#state{connections = NewConns}};
handle_info({new_conn, Port, Time}, State = #state{connections = Conns}) ->
  case conn_sup:start_connection(Port) of
    {ok, Pid} ->
      MRef = erlang:monitor(process, Pid),
      {noreply, State#state{connections = [{Port, MRef, Pid} | Conns]}};
    _ ->
      BackedOff = Time * (1 + rand:uniform()),
      erlang:send_after(BackedOff, self(), {new_conn, Port, BackedOff} ), % back off a bit and give time for the socket
      {noreply, State}
  end;
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
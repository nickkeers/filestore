%%%-------------------------------------------------------------------
%%% @author nick
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jun 2018 8:32 PM
%%%-------------------------------------------------------------------
-module(conn_sup).
-author("nick").

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_connection/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_connection(Port :: integer()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start_connection(Port) ->
  supervisor:start_child(?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 1000, period => 3600},

  Child = #{
    id => tcp_conn,
    start => {tcp_conn, start_link, []},
    type => worker
  },

  {ok, {SupFlags, [Child]}}.


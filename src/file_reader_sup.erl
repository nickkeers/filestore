%%%-------------------------------------------------------------------
%%% @doc
%%% Hold our workers for reading files from remote nodes in parallel. One per filename
%%% @end
%%%-------------------------------------------------------------------
-module(file_reader_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_reader/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    id => file_reader,
    start => {file_reader, start_link, []},
    type => worker
  },

  {ok, {SupFlags, [Child]}}.

start_reader(Filename) ->
  supervisor:start_child(?SERVER, [Filename]).
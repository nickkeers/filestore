%%%-------------------------------------------------------------------
%%% @doc
%%% Hold our workers for reading files from remote nodes in parallel. One per filename
%%% @end
%%%-------------------------------------------------------------------
-module(file_reader_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_reader/2]).

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

-spec init(_Args :: term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
  SupFlags = #{strategy => simple_one_for_one, intensity => 1000, period => 3600},

  Child = #{
    id => file_reader,
    start => {file_reader, start_link, []},
    type => worker,
    restart => temporary
  },

  {ok, {SupFlags, [Child]}}.

-spec start_reader(iodata(), pid()) -> {'ok','undefined' | pid()} | {'ok','undefined' | pid(), term()}.
start_reader(Filename, Parent) ->
  supervisor:start_child(?SERVER, [Filename, Parent]).
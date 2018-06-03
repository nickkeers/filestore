%%% @doc
%%% Supervisor for meta_store and the writer + reader fsm supervisors
%%% @end
-module(storage_sup).

-behavior(supervisor).

%% API
-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link(?MODULE, []).

-spec init(_Args :: term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	Children = [
    #{
      id => meta_store,
      start => {meta_store, start_link, []}
    }
  ],
  {ok, {SupFlags, Children}}.

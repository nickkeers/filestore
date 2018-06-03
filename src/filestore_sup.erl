%%%-------------------------------------------------------------------
%% @doc filestore top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(filestore_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================


-spec init(_Args :: term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => rest_for_one, intensity => 1000, period => 3600},
    Children = [
        #{
            id => storage_sup,
            start => {storage_sup, start_link, []},
            type => supervisor
        },
        #{
            id => es3_master_server,
            start => {es3_master_server, start_link, []},
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

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

-spec start_link() -> 'ignore' | {'error', term()} | {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================


-spec init(_Args :: term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init([]) ->
    SupFlags = #{strategy => rest_for_one, intensity => 1000, period => 3600},
    Children = [
        #{
            id => store,
            start => {store, start_link, []},
            type => worker
        },
        #{
            id => file_reader_sup,
            start => {file_reader_sup, start_link, []},
            type => supervisor
        }
    ],
    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

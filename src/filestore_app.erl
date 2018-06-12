-module(filestore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(application:start_type(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    RestPort = application:get_env(filestore, rest_port, 8100),

    Dispatch = cowboy_router:compile([
        {'_', [
            %% All files
            {"/metadata", metadata_rest_handler, [get_meta]},

            %% Get metadata for a specific filename
            {"/metadata/:filename", metadata_rest_handler, [get_file]},

            % Get a list of all files from all nodes
            {"/metadata/:filename/all", metadata_rest_handler, [get_all_meta]}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(rest_http_listener, [{port, RestPort}], #{env => #{dispatch => Dispatch}}),

    filestore_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
-module(filestore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(application:start_type(), term()) -> {ok, pid()} | {ok, pid(), term()} | {error, term()}.
start(_StartType, _StartArgs) ->
    {ok, RestPort} = application:get_env(filestore, rest_port),
    Dispatch = cowboy_router:compile([
        {'_', [
            %% All files
            {"/metadata", metadata_rest_handler, []},
            %% Get metadata for a specific filename
            {"/metadata/:filename", metadata_rest_handler, []},
            % Get metdata for a file from a certain node
            {"/metdata/:node/:filename", metadata_rest_handler, []},

            {"/all/files", metadata_rest_handler, []},
            %
            {"/all/files/:filename", file_rest_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(rest_http_listener, [{port, RestPort}], #{env => #{dispatch => Dispatch}}),

    filestore_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
-module(metadata_rest_handler).

-export([
    init/2
]).

-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(PUT, <<"PUT">>).
-define(DELETE, <<"DELETE">>).

init(Req, _State) ->

    {cowboy_rest, Req, []}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"PUT">>, <<"DELETE">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/plain">>, to_text}
    ], Req, State}.

route_methods() ->
    #{
        metadata => [?GET],
        metadata_filename => [?GET, ?POST]
    }.

%%
encode(Data, <<"application/json">>) -> ok;
encode(Data, <<"text/plain">>) -> ok.
%%

% /all/metadata
metadata() -> ok.

% /all/metadata/:filename
metadata_filename(undefined, _, _) ->
    #{};
metadata_filename(Filename, _Req, ContentType) ->
    Data = "",
    encode(Data, ContentType).

metadata_node_filename({Node, File}, _Req, _ContentType) -> ok.

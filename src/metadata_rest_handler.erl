-module(metadata_rest_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    do_route/2
]).

-define(GET, <<"GET">>).
-define(POST, <<"POST">>).
-define(PUT, <<"PUT">>).
-define(DELETE, <<"DELETE">>).

init(Req, Opts) ->
    [Op | _] = Opts,
    {cowboy_rest, Req, Op}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, do_route}
    ], Req, State}.

do_route(Req, Op) ->
    case Op of
        get_meta -> get_metadata(Req);
        get_file -> get_file(Req);
        get_all_meta -> get_all_meta(Req);
        _ -> {<<"No such route">>, Req, Op}
    end.

%% ===============================


% /metadata
get_metadata(Req) ->
    Result = store:get_all_chunk_entries(),
    encode_entries(Result, Req, get_metadata).

% /metadata/:filename
get_file(Req) ->
    Filename = cowboy_req:binding(filename, Req),

    case Filename of
        undefined ->
            {<<"Error, no filename given">>, Req, get_file};
        _ ->
            % {{iodata(), integer()}, integer()}
            Result = store:entries_for_filename(Filename),
            encode_entries(Result, Req, get_file)
    end.

encode_entries({error, _}, Req, State) ->
    Encoded = jsone:encode(#{
        error => <<"Error, no entries for filename">>
    }),
    {Encoded, Req, State};
encode_entries(Entries, Req, State) ->
    Result =
        lists:foldl(fun({{Filename, Index}, Checksum}, Acc) ->
            maps:update_with(Filename,
                fun(Indexes) ->
                    maps:put(Index, #{checksum => Checksum}, Indexes)
                end, #{Index => #{checksum => Checksum}}, Acc)
        end, #{}, Entries),
    Encoded = jsone:encode(Result),
    {Encoded, Req, State}.

node_and_filename(Node, Filename) when (Node == undefined) orelse (Filename == undefined) ->
    undefined;
node_and_filename(Node, Filename) ->
    {Node, Filename}.

% /metadata/:filename/all - all entries across all nodes for a file
get_all_meta(Req) ->
    Filename = cowboy_req:binding(filename, Req),
    case Filename of
        undefined ->
            {<<"Error, no entries for given filename">>, Req, get_all_meta};
        _ ->
            Result = store:all_entries_for_filename(Filename),
            encode_entries(Result, Req, get_metadata)
    end.
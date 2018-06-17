-module(metadata_rest_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    do_route/2
]).

-include("rest.hrl").

-spec init(Req :: cowboy_req:req(), Op :: atom()) -> {atom(), cowboy_req:req(), atom()}.
init(Req, Op) ->
    {cowboy_rest, Req, Op}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: atom()) -> {[binary()], cowboy_req:req(), atom()}.
allowed_methods(Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: atom()) -> {[{binary(), atom()}], cowboy_req:req(), atom()}.
content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, do_route}
    ], Req, State}.

-spec do_route(Req :: cowboy_req:req(), Op :: atom()) -> cowboy_req:req().
do_route(Req, Op) ->
    {Body, Req2, Code} =
        case Op of
            get_meta -> get_metadata(Req);
            get_file -> get_file(Req);
            get_all_meta -> get_all_meta(Req);
            _ -> {?JSON_ERROR(<<"No such route">>), Req, Op}
        end,
    cowboy_req:reply(Code, ?JSON_HEADER, jsone:encode(Body), Req2).

%% ===========================================================================================

% /metadata
-spec get_metadata(cowboy_req:req()) -> route_return().
get_metadata(Req) ->
    Result = store:get_all_chunk_entries(),
    case maps:size(Result)  of
        0 ->
            {?JSON_ERROR(<<"Error, no data found">>), Req, 404};
        _ ->
            {Result, Req, 200}
    end.

% /metadata/:filename
-spec get_file(cowboy_req:req()) -> route_return().
get_file(Req) ->
    Filename = cowboy_req:binding(filename, Req),
    case Filename of
        undefined ->
            {?JSON_ERROR(<<"Error, no filename given">>), Req, 404};
        _ ->
            % {{iodata(), integer()}, integer()}
            Result = store:entries_for_filename(Filename),
            ct:pal("Result for ~p: ~p", [Filename, Result]),
            encode_entries(Result, Req)
    end.

% /metadata/:filename/all - all entries across all nodes for a file
-spec get_all_meta(Req :: cowboy_req:req()) -> route_return().
get_all_meta(Req) ->
    Filename = cowboy_req:binding(filename, Req),
    case Filename of
        undefined ->
            {?JSON_ERROR(<<"Error, no entries for filename">>), Req, 404};
        _ ->
            Result = store:all_entries_for_filename(Filename),
            encode_entries(Result, Req)
    end.

%% ===========================================================================================
%% Utilities
%% ===========================================================================================

-spec encode_entries({'error', term()} | list({iodata(), integer()}), cowboy_req:req()) -> route_return().
encode_entries({error, _}, Req) ->
    {?JSON_ERROR(<<"Error, no entries for filename">>), Req, 404};
encode_entries(Entries, Req) ->
    case length(Entries) of
        0 ->
            {?JSON_ERROR(<<"Error, no entres for filename">>), Req, 404};
        _ ->
            Result =
                lists:foldl(fun({{Filename, Index}, Checksum}, Acc) ->
                    BIndex = integer_to_binary(Index),
                    maps:update_with(Filename,
                        fun(Indexes) ->
                            maps:put(BIndex, #{checksum => Checksum}, Indexes)
                        end, #{BIndex => #{checksum => Checksum}}, Acc)
                            end, #{}, Entries),
            {Result, Req, 200}
    end.
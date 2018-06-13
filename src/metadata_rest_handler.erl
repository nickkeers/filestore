-module(metadata_rest_handler).

-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    do_route/2
]).

-include("rest.hrl").

init(Req, Op) ->
    {cowboy_rest, Req, Op}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, do_route}
    ], Req, State}.

do_route(Req, Op) ->
    {Body, Req2, Code} =
        case Op of
            get_meta -> get_metadata(Req);
            get_file -> get_file(Req);
            get_all_meta -> get_all_meta(Req);
            _ -> {jsone:encode(#{error => <<"No such route">>}), Req, Op}
        end,
    cowboy_req:reply(Code, ?JSON_HEADER, Body, Req2).

%% ===========================================================================================

-type entries_return() :: {binary(), cowboy_req:req(), integer()}.

% /metadata
-spec get_metadata(cowboy_req:req()) -> entries_return().
get_metadata(Req) ->
    case store:get_all_chunk_entries() of
        {error, _Reason} ->
            {jsone:encode(#{error => <<"Error, no filename given">>}), Req, 404};
        Result ->
            Encoded = jsone:encode(Result),
            {Encoded, Req, 200}
    end.

% /metadata/:filename
-spec get_file(cowboy_req:req()) -> entries_return().
get_file(Req) ->
    Filename = cowboy_req:binding(filename, Req),
    case Filename of
        undefined ->
            {jsone:encode(#{error => <<"Error, no filename given">>}), Req, 404};
        _ ->
            % {{iodata(), integer()}, integer()}
            Result = store:entries_for_filename(Filename),
            encode_entries(Result, Req)
    end.

% /metadata/:filename/all - all entries across all nodes for a file
-spec get_all_meta(Req :: cowboy_req:req()) -> entries_return().
get_all_meta(Req) ->
    Filename = cowboy_req:binding(filename, Req),
    case Filename of
        undefined ->
            {jsone:encode(#{error => <<"Error, no entries for given filename">>}), Req, 404};
        _ ->
            Result = store:all_entries_for_filename(Filename),
            encode_entries(Result, Req)
    end.

%% ===========================================================================================
%% Utilities
%% ===========================================================================================

-spec encode_entries({'error', term()} | list({iodata(), integer()}), cowboy_req:req()) -> {binary(), cowboy_req:req(), integer()}.
encode_entries({error, _}, Req) ->
    Encoded = jsone:encode(#{
        error => <<"Error, no entries for filename">>
    }),
    {Encoded, Req, 404};
encode_entries(Entries, Req) ->
    Result =
        lists:foldl(fun({{Filename, Index}, Checksum}, Acc) ->
            BIndex = integer_to_binary(Index),
            maps:update_with(Filename,
                fun(Indexes) ->
                    maps:put(BIndex, #{checksum => Checksum}, Indexes)
                end, #{BIndex => #{checksum => Checksum}}, Acc)
                    end, #{}, Entries),
    Encoded = jsone:encode(Result),
    {Encoded, Req, 200}.
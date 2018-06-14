-module(chunk_rest_handler).

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
    Method = cowboy_req:method(Req),
    {Body, Req2, Code} =
        case Op of
            read_chunk -> read_chunk(Req, Method);
            _ -> {?JSON_ERROR(<<"No such route">>), Req, Op}
        end,
    cowboy_req:reply(Code, ?JSON_HEADER, jsone:encode(Body), Req2).

%% ------------------------------------------------------
%% Route functions
%% ------------------------------------------------------

-spec read_chunk(Req :: cowboy_req:req(), Method :: binary()) -> route_return().
read_chunk(Req, <<"GET">>) ->
    case cowboy_req:binding(filename, Req) of
        undefined ->
            {?JSON_ERROR(<<"Error, filename not provided">>), Req, 400};
        Filename ->
            case cowboy_req:binding(index, Req) of
                undefined ->
                    {?JSON_ERROR(<<"Error, chunk index not provided">>), Req, 400};
                Index ->
                    case es3_chunk:read({Filename, Index}) of
                        {error, _Reason} ->
                            {?JSON_ERROR(<<"Could not read chunk">>), Req, 404};
                        ChunkContents ->
                            {#{
                                filename => Filename,
                                chunk_index => Index,
                                contents => ChunkContents
                            }, Req, 200}
                    end
            end
    end;
read_chunk(Req, _) ->
    {?JSON_ERROR(<<"Error, invalid request method, only GET is supported">>), Req, 400}.

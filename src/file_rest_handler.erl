-module(file_rest_handler).

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
    Methods = [<<"GET">>, <<"DELETE">>, <<"POST">>],
    {Methods, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: atom()) -> {[{binary(), atom()}], cowboy_req:req(), atom()}.
content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, do_route}
    ], Req, State}.

-spec do_route(Req :: cowboy_req:req(), State :: any()) -> cowboy_req:req().
do_route(Req, _State) ->
    Method = cowboy_req:method(Req),
    {Body, Req2, Code} = do_file_route(Req, Method),
    cowboy_req:reply(Code, ?JSON_HEADER, jsone:encode(Body), Req2).

%% ------------------------------------------------------
%% Route functions
%% ------------------------------------------------------

-spec do_file_route(Req :: cowboy_req:req(), Method :: binary()) -> route_return().
%% Create a new file
do_file_route(Req, <<"POST">>) ->
    case cowboy_req:has_body(Req) of
        false ->
            {?JSON_ERROR(<<"Can't create file, no request body provided">>), Req, 400};
        true ->
            case cowboy_req:binding(filename, Req) of
                undefined ->
                    {?JSON_ERROR(<<"Can't create file, no filename provided">>), Req, 400};
                Name ->
                    {ok, Body, Req2} = fetch_body(Req, <<>>),

                    case es3:write(Name, Body) of
                        ok ->
                            {#{msg => <<"Created new file">>}, Req2, 200};
                        {error, exists} ->
                            {?JSON_ERROR(<<"Error writing file">>), Req2, 409};
                        {error, _} ->
                            {?JSON_ERROR(<<"Error writing file">>), Req2, 500}
                    end
            end
    end;
%% Get the file contents
do_file_route(Req, <<"GET">>) ->
    Filename = cowboy_req:binding(filename, Req),

    case Filename of
        undefined ->
            {?JSON_ERROR(<<"Error, filename not provided">>), Req, 400};
        Filename ->
            case es3:read(Filename) of
                {error, _Reason} ->
                    {?JSON_ERROR(<<"Could not read file">>), Req, 404};
                Result ->
                    {#{
                        filename => Filename,
                        contents => Result
                    }, Req, 200}
            end
    end;
do_file_route(Req, _) ->
    {?JSON_ERROR(<<"Error, invalid request method, only GET is supported">>), Req, 405}.

-spec fetch_body(Req0 :: cowboy_req:req(), Acc :: binary()) -> {ok, binary(), cowboy_req:req()}.
fetch_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            fetch_body(Req, <<Acc/binary, Data/binary>>)
    end.

%%multipart(Req0) ->
%%    case cowboy_req:read_part(Req0) of
%%        {ok, _Headers, Req1} ->
%%            {ok, _Body, Req} = cowboy_req:read_part_body(Req1),
%%            multipart(Req);
%%        {done, Req} ->
%%            Req
%%    end.

-module(file_rest_handler).

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
    Methods = [<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"POST">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, do_route}
    ], Req, State}.

%%{"/file/:filename", file_rest_handler, get_file_contents},
%%{"/file/new", file_rest_handler, new_file}

do_route(Req, Op) ->
    Method = cowboy_req:method(Req),
    {Body, Req2, Code} =
        case Op of
            read_file -> get_file_contents(Req, Method);
            new_file -> new_file(Req, Method);
            _ -> {<<"No such route">>, Req, Op}
        end,
    cowboy_req:reply(Code, ?JSON_HEADER, Body, Req2).

%% ------------------------------------------------------
%% Route functions
%% ------------------------------------------------------

-spec get_file_contents(Req :: cowboy_req:req(), Method :: binary()) -> {binary(), cowboy_req:req(), integer()}.
get_file_contents(Req, <<"GET">>) ->
    Filename = cowboy_req:binding(filename, Req),

    case Filename of
        undefined ->
            {jsone:encode(#{error => <<"Error, filename not provided">>}), Req, 400};
        Filename ->
            case es3:read(Filename) of
                {error, _Reason} ->
                    {jsone:encode(#{error => <<"Could not read file">>}), Req, 404};
                Result ->
                    {jsone:encode(#{
                        filename => Filename,
                        contents => Result
                    }), Req, 200}
            end
    end;
get_file_contents(Req, _) ->
    {jsone:encode(#{error => <<"Error, invalid request method, only GET is supported">>}), Req, 400}.

-spec new_file(Req :: cowboy_req:req(), Method :: binary()) -> {binary(), cowboy_req:req(), integer()}.
new_file(Req, Method) when Method == <<"PUT">> orelse Method == <<"POST">> ->
    Qs = cowboy_req:parse_qs(Req),
    HasName = lists:keymember(<<"name">>, 1, Qs),
    HasBody = cowboy_req:has_body(Req),

    case HasName of
        false ->
            {jsone:encode(#{error => <<"Can't create file, no name provided in query string">>}), Req, 400};
        true ->
            case HasBody of
                true ->
                    Body = fetch_body(Req, <<>>),
                    {_, Name} = lists:keyfind(<<"name">>, 1, Qs),

                    case es3:write(Name, Body) of
                        ok ->
                            {jsone:encode(#{msg => <<"Created new file">>}), Req, 200};
                        {error, _} ->
                            {jsone:encode(#{error => <<"Error writing file">>}), Req, 400}
                    end;
                false ->
                    {jsone:encode(#{error => <<"Can't create file, no request body provided">>}), Req, 400}
            end
    end.

fetch_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            fetch_body(Req, <<Acc/binary, Data/binary>>)
    end.
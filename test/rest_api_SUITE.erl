-module(rest_api_SUITE).

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_testcase/2,
    end_per_suite/1,
    test_metadata/1,
    test_metadata_all/1,
    test_failed_metadata/1,
    test_chunk_endpoint/1
]).

all() ->
    [
        test_metadata,
        test_metadata_all,
        test_failed_metadata,
        test_chunk_endpoint
    ].

-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:ensure_all_started(hackney),

    application:set_env(filestore, chunk_size, 100),
    application:set_env(filestore, rest_port, 8100),
    application:ensure_all_started(filestore),

    Config.

end_per_suite(Config) ->
    store:close_tables(),
    application:stop(filestore),
    Config.

end_per_testcase(_, Config) ->
    Config.

assert_key_size(Size, Key, Decoded) ->
    ?assertEqual(Size, maps:size(maps:get(Key, Decoded))).

rest_get(StatusCode, Url) ->
    {ok, StatusCode2, _RespHeader, ClientRef} = hackney:request(get, Url, [], <<>>, []),
    ?assertEqual(StatusCode, StatusCode2),

    {ok, Body} = hackney:body(ClientRef),
    Decoded = jsone:decode(Body, [{object_format, map}]),

    Decoded.

%% ----------------------------------------------------------

test_metadata(_Config) ->
    Data = crypto:strong_rand_bytes(1000),

    ?assertEqual(ok, es3:write(<<"test.txt">>, Data)),

    Url = <<"localhost:8100/metadata/test.txt">>,
    Decoded = rest_get(200, Url),

    %?assertEqual(10, maps:size(maps:get(<<"test.txt">>, Decoded))),
    assert_key_size(10, <<"test.txt">>, Decoded),

    ?assertEqual(ok, es3:delete(<<"test.txt">>)),

    ok.

test_failed_metadata(_Config) ->
    Url = <<"localhost:8100/metadata/not_real_key">>,
    Decoded = rest_get(404, Url),

    Error = #{
        <<"error">> => <<"Error, no entries for filename">>
    },

    ?assertEqual(Decoded, Error).


%% /all/metadata route test

test_metadata_all(_Config) ->
    Data = crypto:strong_rand_bytes(1000),

    FileName1 = <<"test.txt">>,
    FileName2 = <<"test2.txt">>,

    ?assertEqual(ok, es3:write(FileName1, Data)),
    ?assertEqual(ok, es3:write(FileName2, Data)),

    Url = <<"localhost:8100/metadata">>,
    Decoded = rest_get(200, Url),

    ?assertEqual(2, maps:size(Decoded)), % we have 2 files

    ?assertEqual(ok, es3:delete(FileName1)),
    ?assertEqual(ok, es3:delete(FileName2)),

    %% After deleting, no files should be left in the metadata
    Decoded2 = rest_get(404, Url),
    ?assertEqual(maps:size(Decoded2), 1),
    ?assertEqual(maps:keys(Decoded2), [<<"error">>]),

    ok.

test_chunk_endpoint(_Config) ->
    Data = crypto:strong_rand_bytes(1000),

    FileName1 = <<"test.txt">>,
    FileName2 = <<"test2.txt">>,

    ?assertEqual(ok, es3:write(FileName1, Data)),
    ?assertEqual(ok, es3:write(FileName2, Data)),

    Url = <<"localhost:8100/chunk/test.txt/1">>,
    Decoded = rest_get(200, Url),

    ct:pal(Decoded),
    ok.


%%test_create_file(_Config) ->
%%    Data = <<"This is a super secret file with super secret text so that we can post to cowboy">>,
%%
%%    FileName = <<"created_file.txt">>,
%%
%%    Url = <<"localhost:8100/file/created_file.txt">>,
%%
%%    {FormData, FinalSize} = hackney_multipart:encode_form([{<<"data">>, Data}]),
%%    {ok, 200, _, ClientRef} = hackney:request(post, Url, [{"Content-length", FinalSize}, {"Content-type", "multipart/form-data"}], {multipart, [{<<"data">>, FormData}]}, []),
%%
%%    {ok, Body} = hackney:body(ClientRef),
%%    Decoded = jsone:decode(Body, [{object_format, map}]),
%%
%%    Result = #{
%%        <<"msg">> => <<"Created new file">>
%%    },
%%
%%    ?assertEqual(Decoded, Result),
%%
%%    Url2 = <<"localhost:8100/metadata">>,
%%    Decoded2 = rest_get(200, Url2),
%%
%%    ?assertEqual(1, maps:size(Decoded2)), % we have 2 files
%%    es3:delete(FileName),
%%    ok.

-module(store_dist_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0,
    init_per_suite/1,
    end_per_suite/1,
    end_per_testcase/2
]).

-export([
    test_write/1,
    test_read/1,
    test_delete/1,
    test_failed_read/1
]).

-define(TEST_FILE, <<"test.txt">>).

all() ->
    [
        test_write,
        test_read,
        test_delete,
        test_failed_read
    ].

init_per_suite(Config) ->
    application:ensure_all_started(filestore),
    application:set_env(filestore, chunk_size, 100),

    Nodes = start_slaves(),
    [{nodes, Nodes} | Config].

end_per_suite(Config) ->
    application:stop(filestore),
    Config.

end_per_testcase(_, Config) ->
    ok = es3:delete(?TEST_FILE),
    Config.

%% ------------------------------

% Test if we can write a file
test_write(_Config) ->
    Data = crypto:strong_rand_bytes(1000),
    ?assertEqual(ok, es3:write(?TEST_FILE, Data)).

% Test if we can read a file
test_read(_Config) ->
    Data = crypto:strong_rand_bytes(1000),
    ?assertEqual(ok, es3:write(?TEST_FILE, Data)),
    ?assertEqual(Data, es3:read(?TEST_FILE)).

test_delete(_Config) ->
    Data = crypto:strong_rand_bytes(1000),
    ?assertEqual(ok, es3:write(?TEST_FILE, Data)),
    ?assertEqual(ok, es3:delete(?TEST_FILE)).

test_failed_read(_Config) ->
    ?assertEqual({error, no_entries}, es3:read(<<"missing">>)).

%% ------------------------------
start_slaves() ->
    SlaveNodes = [es3_2, es3_3, es3_4],
    PortStarts = [10000, 11000, 12000],
    start_slaves(SlaveNodes, PortStarts, []).

start_slaves([], _, Acc) ->
    Acc;
start_slaves([Node | Nodes], [PortStart | PortStarts], Acc) ->
    ErlFlags = "-config ../../../../test/distributed_test_config.config",
    Path = code:get_path(),

    ct:pal("Starting ~p~n", [Node]),
    {ok, HostNode} = ct_slave:start(Node,
        [
            {kill_if_fail, true}, {monitor_master, true},
            {init_timeout, 5000}, {startup_timeout, 5000},
            {startup_functions, [
                {application, set_env, [filestore, port_start, PortStart]},
                {application, set_env, [filestore, rest_port, PortStart + 1]}
            ]},
            {erl_flags, ErlFlags}
        ]),

    ct:pal("Started node: ~p~n", [HostNode]),
    pong = net_adm:ping(HostNode),

    rpc:call(HostNode, code, add_paths, [Path]),
    {ok, _} = rpc:call(HostNode, application, ensure_all_started, [filestore]),

    start_slaves(Nodes, PortStarts, [HostNode | Acc]).

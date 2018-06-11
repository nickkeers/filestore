-module(store_dist_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0,
    init_per_suite/1,
    end_per_suite/1]).

-export([
    test_write/1
]).

all() ->
    [
        test_write
    ].

init_per_suite(Config) ->
    application:ensure_all_started(filestore),
    Nodes = start_slaves(),
    [{nodes, Nodes} | Config].

end_per_suite(Config) ->
    application:stop(filestore),
    Config.

%% ------------------------------

test_write(_Config) ->
    Data = crypto:strong_rand_bytes(1000),
    ct:pal("Write: ~p~n", [Data]),
    ok = es3:write("test.txt", Data),

    Results = es3:read("test.txt"),
    ct:pal("Results: ~p~n", [Results]),
    ok.


%% ------------------------------
start_slaves() ->
    SlaveNodes = [es3_2, es3_3, es3_4],
    PortStarts = [10000, 11000, 12000],
    start_slaves(SlaveNodes, PortStarts, []).

start_slaves([], _, Acc) ->
    Acc;
start_slaves([Node | Nodes], [PortStart | PortStarts], Acc) ->
    ErlFlags = "-config ../../distributed_test_config.config",
    Path = code:get_path(),

    ct:pal("Starting ~p~n", [Node]),
    {ok, HostNode} = ct_slave:start(Node,
        [
            {kill_if_fail, true}, {monitor_master, true},
            {init_timeout, 5000}, {startup_timeout, 5000},
            {startup_functions, [
                {application, set_env, [filestore, port_start, PortStart]}
            ]},
            {erl_flags, ErlFlags}
        ]),

    ct:pal("Started node: ~p~n", [HostNode]),
    pong = net_adm:ping(HostNode),

    rpc:call(HostNode, code, add_paths, [Path]),

    {ok, _} = rpc:call(HostNode, application, ensure_all_started, [filestore]),

    start_slaves(Nodes, PortStarts, [HostNode | Acc]).

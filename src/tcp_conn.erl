-module(tcp_conn).

%% API
-export([
    start_link/1
]).

start_link(Port) ->
    {ok, Sock} = gen_tcp:listen(Port, [{reuseaddr, true}, binary, {active, once}]),
    Pid = proc_lib:spawn_link(fun loop/1),
    gen_tcp:controlling_process(Sock, Pid),
    {ok, Pid}.

loop(Socket) ->
    {ok, Accepted} = gen_tcp:accept(Socket),

    receive
        {tcp, Accepted, Data} ->
            {{Filename, ChunkIndex}, Chunk, Checksum} = binary_to_term(Data),
            OurChecksum = es3_chunk:checksum(Chunk),

            case Checksum =:= OurChecksum of
                true ->
                    es3_chunk:write({Filename, ChunkIndex}, Chunk);
                false ->
                    io:format("[tcp_conn] Not writing file: ~s, chunk index: ~b. Checksums do not match: ~p vs ~p~n",
                        [Filename, ChunkIndex, Checksum, OurChecksum]),
                    gen_tcp:send(Accepted, term_to_binary({error, mismatched_checksums}))
            end,

            gen_tcp:close(Accepted),
            inet:setopts(Socket, [{active, once}]),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("[tcp_conn ~p] Socket ~w closed~n", [self(), Socket]),
            exit(1)
    end.
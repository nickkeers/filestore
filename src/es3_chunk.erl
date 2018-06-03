-module(es3_chunk).

%% Main API
-export([
  write/2,
  read/1,
  delete/1
]).

%% Needed functions
-export([
  checksum/1
]).

-type chunk_name() :: {integer(), integer()}. % {Index, Checksum}.
-export_type([chunk_name/0]).

-spec write(Key :: any(), Chunk :: binary()) -> ok | {error, Reason :: any()}.
write(Key, Chunk) ->
  ok.


-spec read(Key :: any()) -> Chunk :: binary() | {error, Reason :: any()}.
read(Key) ->
  <<"">>.


-spec delete(Key :: any()) -> ok | {error, Reason :: any()}.
delete(Key) ->
  ok.


-spec checksum(Chunk :: binary()) -> integer().
checksum(Chunk) ->
  erlang:crc32(Chunk).


%% ----------------------------------
%% Unit tests
%% ----------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



-endif.
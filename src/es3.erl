-module(es3).

%% API
-export([
  write/2,
  read/1,
  delete/1
]).

-spec write(Name, Object) -> Res when
  Name :: iodata(),
  Object :: binary(),
  Res :: ok | {error, Reason :: any()}.

write(Name, Object) ->
  ok.

-spec read(Name) -> Object when
  Name :: iodata(),
  Object :: binary() | {error, Reason :: any()}.
read(Name) ->
  <<"">>.

-spec delete(Name) -> Res when
  Name :: iodata(),
  Res :: ok | {error, Reason :: any()}.
delete(Name) ->
  ok.


%% ----------------------------------
%% Unit tests
%% ----------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").



-endif.
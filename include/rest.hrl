-define(JSON_HEADER, #{<<"Content-Type">> => <<"application/json">>}).
-define(JSON_ERROR(Error), #{error => Error}).

-export_type([route_return/0]).

-type route_return() :: {map(), cowboy_req:req(), integer()}.
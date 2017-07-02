-module(prometheus_cowboy).

-export([to_cowboy_headers/1]).

%% ===================================================================
%% API
%% ===================================================================

to_cowboy_headers(RespHeaders) ->
  lists:map(fun to_cowboy_headers_/1, RespHeaders).

%% ===================================================================
%% Private functions
%% ===================================================================

to_cowboy_headers_({Name, Value}) ->
  {to_cowboy_name(Name), Value}.

to_cowboy_name(Name) ->
  binary:replace(atom_to_binary(Name, utf8), <<"_">>, <<"-">>).

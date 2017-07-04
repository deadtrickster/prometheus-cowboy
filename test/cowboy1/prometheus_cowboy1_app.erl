%% @private
-module(prometheus_cowboy1_app).

%% API.
-export([start/0]).
%% API.

start() ->  
  prometheus_http_impl:setup(),
  
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/metrics/[:registry]", prometheus_cowboy1_handler, []},                                           
                                           {"/", toppage_handler, []}
                                          ]}
                                   ]),
  {ok, _Listener} = cowboy:start_http(http, 100, [{port, 0}], [
                                                               {env, [{dispatch, Dispatch}]}
                                                              ]),
  ranch:get_port(http).

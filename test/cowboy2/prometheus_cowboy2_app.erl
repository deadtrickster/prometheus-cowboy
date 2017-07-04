%% @private
-module(prometheus_cowboy2_app).

%% API.
-export([start/0]).
%% API.

start() ->
  prometheus_http_impl:setup(),

  Routes = [
            {'_', [
                   {"/metrics/[:registry]", prometheus_cowboy2_handler, []},
                   {"/", toppage_handler, []}
                  ]}
           ],
  Dispatch = cowboy_router:compile(Routes),
  {ok, _Listener} = cowboy:start_clear(http, [{port, 0}],
                                       #{env => #{dispatch => Dispatch}}),
  ranch:get_port(http).

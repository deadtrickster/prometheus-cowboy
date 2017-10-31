%% @doc
%% Cowboy2 handler for exporting prometheus metrics.
%% @end
-module(prometheus_cowboy1_handler).

%% -behaviour(cowboy_http_handler).

-export([init/3,
         handle/2,
         terminate/3]).

%% ===================================================================
%% cowboy_http_handler callbacks
%% ===================================================================

init(_Transport, Request, Opts) ->
  {ok, Request, Opts}.

handle(Request0, State) ->
  {Method, Request} = cowboy_req:method(Request0),
  {ok, Response} = gen_response(Method, Request),
  {ok, Response, State}.

terminate(_Reason, _Request, _State) ->
  ok.

%% ===================================================================
%% Private functions
%% ===================================================================

gen_response(<<"GET">>, Request) ->
  {Registry0, _} = cowboy_req:binding(registry, Request, <<"default">>),
  case prometheus_registry:exists(Registry0) of
    false ->
      cowboy_req:reply(404, [], <<"Unknown Registry">>, Request);
    Registry ->
      gen_metrics_response(Registry, Request)
  end;
gen_response(_, Request) ->
  Request.

gen_metrics_response(Registry, Request) ->
  URI = true,
  GetHeader = fun (Name, Default) ->
                  {Value, _} = cowboy_req:header(iolist_to_binary(Name),
                                                 Request, Default),
                  Value
              end,
  {Code, RespHeaders, Body} = prometheus_http_impl:reply(
                                #{path => URI,
                                  headers => GetHeader,
                                  registry => Registry,
                                  standalone => false}),

  Headers = prometheus_cowboy:to_cowboy_headers(RespHeaders),
  cowboy_req:reply(Code, Headers, Body, Request).

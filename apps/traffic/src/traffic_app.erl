-module(traffic_app).
-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1
]).


%% API
-export([dispatch_rules/0]).


%% ===================================================================
%% API functions
%% ===================================================================

dispatch_rules() ->
  cowboy_router:compile([
    {'_', [
      {"/", hello_handler, []},
      {"/sequence/create", sequence_handler, []},
      {'_', not_found_handler, []}
    ]}
  ]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->
  Dispatch = dispatch_rules(),
  Port = 8080,
  {ok, _} = cowboy:start_http(my_http_listener, 100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  traffic_sup:start_link().

stop(_State) ->
  ok.

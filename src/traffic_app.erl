-module(traffic_app).
-behaviour(application).

-include("server_config.hrl").

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
      {"/sequence/create", sequence_handler, []},
      {"/observation/add", observation_handler, []},
      {"/clear", clear_handler, []},
      {'_', not_found_handler, []}
    ]}
  ]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->
  %% @todo: добавить поднятие ets после падения
  sequence_processor:start(),

  Dispatch = dispatch_rules(),
  Port = ?PORT,
  {ok, _} = cowboy:start_http(my_http_listener, 100,
    [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  traffic_sup:start_link().

stop(_State) ->
  sequence_processor:save(),
  ok.

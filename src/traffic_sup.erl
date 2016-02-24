-module(traffic_sup).
-behaviour(supervisor).

-export([
  start_link/0,
  init/1
]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Procs = [],
  {ok, {{one_for_one, 1, 5}, Procs}}.

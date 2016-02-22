-module(traffic_sup).
-behaviour(supervisor).

-include("recdef.hrl").

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  sequence_processor:start(),
  Procs = [],
%%  @todo: узнать почему не работает?
%%  Procs = [
%%    {traffic_server, {traffic_server, start_link, []},
%%      permanent, 5000, worker, [traffic_server]}
%%  ],
%%  {ok, {{one_for_one, 10, 10}, Procs}}.
  {ok, {{one_for_one, 1, 5}, Procs}}.

-module(clear_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  sequence_processor:clear(),
  Body = <<"{\"status\": \"ok\", \"response\": \"ok\"}">>,
  {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

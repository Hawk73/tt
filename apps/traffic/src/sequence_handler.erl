-module(sequence_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  %%  TODO: запомнить uuid
  Uuid = uuid:to_string(uuid:uuid1()),
  Body = erlang:iolist_to_binary([<<"{\"status\": \"ok\", \"response\": {\"sequence\": \"">>, Uuid, <<"\"}}">>]),
  {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

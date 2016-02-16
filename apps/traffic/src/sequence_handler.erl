-module(sequence_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  Body = case sequence_processor:create_uuid() of
    {Uuid, true} ->
      sequence_responses:ok(Uuid);
    _ ->
      responses:error(<<"Internal error">>)
  end,
  {ok, Req2} = cowboy_req:reply(200, [], Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

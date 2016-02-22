-module(not_found_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  Body = <<"<h1>404 Page Not Found</h1>">>,
  {ok, Req2} = cowboy_req:reply(404, [], Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

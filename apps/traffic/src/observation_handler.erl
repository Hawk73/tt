-module(observation_handler).
-behaviour(cowboy_http_handler).

-include("recdef.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.


handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = process(Method, HasBody, Req2),
  {ok, Req3, State}.


process(<<"POST">>, true, Req) ->
  %% @todo: Может заюзать опцию content_decode для парсинга?
  {ok, ReqBody, _Req2} = cowboy_req:body(Req),
  DecodedReqBody = jsx:decode(ReqBody),
  Body = process_data(DecodedReqBody),
  cowboy_req:reply(200, [], Body, Req);
process(<<"POST">>, false, Req) ->
  Body = responses:error(<<"Missing body.">>),
  cowboy_req:reply(400, [], Body, Req);
process(_, _, Req) ->
  Body = responses:error(<<"Method not allowed.">>),
  cowboy_req:reply(405, [], Body, Req).


%% @todo: провалидировать цвет
process_data(_Data = [
  {<<"observation">>, [{<<"color">>, Color}, {<<"numbers">>, Numbers}]},
  {<<"sequence">>, Uuid}]
) ->
  case observation_processor:perform(#indication{color=Color, numbers=Numbers, uuid=Uuid}) of
    {ok, [StartNumbers, MissingSections]} ->
      observation_responses:ok(StartNumbers, MissingSections);
    {error, Msg} ->
      responses:error(Msg)
  end;
process_data(InvalidData) ->
  error_logger:info_msg("Invalid data: ~p~n", InvalidData),
  responses:error(<<"Invalid format.">>).


terminate(_Reason, _Req, _State) ->
  ok.

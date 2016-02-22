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
  DecodedReqBody = decode(ReqBody),
  Body = case parse_indication(DecodedReqBody) of
    {ok, Indication} -> process_data(Indication);
     _ -> responses:error(<<"Invalid format">>, DecodedReqBody)
  end,
  cowboy_req:reply(200, [], Body, Req);
process(<<"POST">>, false, Req) ->
  Body = responses:error(<<"Missing body">>),
  cowboy_req:reply(400, [], Body, Req);
process(_, _, Req) ->
  Body = responses:error(<<"Method not allowed">>),
  cowboy_req:reply(405, [], Body, Req).


decode(ReqBody) ->
  try jsx:decode(ReqBody) of
    DecodedReqBody -> DecodedReqBody
  catch
    _:_ -> []
  end.



parse_indication(_Data = [
  {<<"observation">>, [{<<"color">>, <<"green">>}, {<<"numbers">>, BinaryDigitStrs}]},
  {<<"sequence">>, Uuid}
]) ->
  case numbers:parse_binary_digits(BinaryDigitStrs) of
    [{ok, FirstEDigit}, {ok, SecondEDigit}] ->
      {ok, #indication{uuid=Uuid, color=green, first_e_digit=FirstEDigit, second_e_digit=SecondEDigit}};
    _ -> error
  end;

parse_indication(_Data = [{<<"observation">>, [{<<"color">>, <<"red">>}]}, {<<"sequence">>, Uuid}]) ->
  {ok, #indication{uuid=Uuid, color=red}};

parse_indication(_Data) -> error.


process_data(Indication = #indication{}) ->
  case observation_processor:perform(Indication) of
    {ok, [StartNumbers, MissingSections]} ->
      observation_responses:ok(StartNumbers, MissingSections);
    {error, Msg} ->
      responses:error(Msg)
  end.


terminate(_Reason, _Req, _State) ->
  ok.

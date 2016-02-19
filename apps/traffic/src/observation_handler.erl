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
  Body = process_data(DecodedReqBody),
  cowboy_req:reply(200, [], Body, Req);
process(<<"POST">>, false, Req) ->
  Body = responses:error(<<"Missing body.">>),
  cowboy_req:reply(400, [], Body, Req);
process(_, _, Req) ->
  Body = responses:error(<<"Method not allowed.">>),
  cowboy_req:reply(405, [], Body, Req).


decode(ReqBody) ->
  try jsx:decode(ReqBody) of
    DecodedReqBody -> DecodedReqBody
  catch
    _:_ -> []
  end.


%% @todo: провалидировать цвет
process_data(_Data = [
  {<<"observation">>, [{<<"color">>, Color}, {<<"numbers">>, BinaryDigitStrs}]},
  {<<"sequence">>, Uuid}
]) ->
  Indication = case numbers:parse_binary_digits(BinaryDigitStrs) of
    [{ok, FirstEDigit}, {ok, SecondEDigit}] ->
      #indication{color=Color, first_e_digit=FirstEDigit, second_e_digit=SecondEDigit, uuid=Uuid};
    _ -> error
  end,

  case observation_processor:perform(Indication) of
    {ok, [StartNumbers, MissingSections]} ->
      observation_responses:ok(StartNumbers, MissingSections);
    {error, Msg} ->
      responses:error(Msg)
  end;
process_data(InvalidData) -> responses:error(<<"Invalid format.">>, InvalidData).


terminate(_Reason, _Req, _State) ->
  ok.

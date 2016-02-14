-module(observation_handler).
-behaviour(cowboy_http_handler).

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
  %% TODO: Может заюзать опцию content_decode для парсинга?
  {ok, ReqBody, _Req2} = cowboy_req:body(Req),
  DecodedReqBody = jsx:decode(ReqBody),
  Body = process_data(DecodedReqBody),
  cowboy_req:reply(200, [], Body, Req);
process(<<"POST">>, false, Req) ->
  Body = error_answer(<<"Missing body.">>),
  cowboy_req:reply(400, [], Body, Req);
process(_, _, Req) ->
  Body = error_answer(<<"Method not allowed.">>),
  cowboy_req:reply(405, [], Body, Req).

process_data(_Data) ->
  error_logger:info_msg("decoded body: ~p~n", [_Data]),
  StartNumbers = [1, 2],
  MissingSections = ["0000000", "1000010"],
  ok_answer(StartNumbers, MissingSections).


ok_answer(StartNumbers, MissingSections) ->
  Pattern = "{\"status\": \"ok\", \"response\": {\"start\": ~lp, \"missing\": ~p}}",
  AnswerList = io_lib:format(Pattern, [StartNumbers, MissingSections]),
  lists:flatten(AnswerList).

error_answer(Msg) ->
  erlang:iolist_to_binary([<<"{\"status\": \"error\", \"msg\": \"">>, Msg, <<"\"}">>]).


terminate(_Reason, _Req, _State) ->
  ok.


%%welcome(Req, State) ->
%%  {ok, ReqBody, Req2} = cowboy_req:body(Req),
%%  Req_Body_decoded = jsx:decode(ReqBody),
%%  [{<<"title">>,Title},{<<"content">>,Content}] = Req_Body_decoded,
%%  Title1 = binary_to_list(Title),
%%  Content1 = binary_to_list(Content),
%%  io:format("Title1 is ~p ~n ", [Title1]),
%%  io:format("Content1 is ~p ~n", [Content1]),
%%  io:format("Title is ~p ~n", [Title]),
%%  io:format("Content is ~p ~n", [Content]),
%%  lager:log(info, [], "Request Body", [Req_Body_decoded]),
%%  Res1 = cowboy_req:set_resp_body(ReqBody, Req2),
%%  Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
%%  Res3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2),
%%  {true, Res3, State}.

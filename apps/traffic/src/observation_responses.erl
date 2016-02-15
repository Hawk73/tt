-module(observation_responses).

-export([
  ok/2,
  error/1
]).

ok(StartNumbers, MissingSections) ->
  Pattern = "{\"status\": \"ok\", \"response\": {\"start\": ~lp, \"missing\": ~p}}",
  AnswerList = io_lib:format(Pattern, [StartNumbers, MissingSections]),
  lists:flatten(AnswerList).

error(Msg) ->
  erlang:iolist_to_binary([<<"{\"status\": \"error\", \"msg\": \"">>, Msg, <<"\"}">>]).

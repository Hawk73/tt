-module(observation_responses).

-export([
  ok/2
]).

ok(StartNumbers, MissingSections) ->
  Pattern = "{\"status\": \"ok\", \"response\": {\"start\": ~lp, \"missing\": ~100p}}",
  AnswerList = io_lib:format(Pattern, [StartNumbers, MissingSections]),
  lists:flatten(AnswerList).

-module(observation_responses).

-export([
  ok/2
]).

ok(StartNumbers, MissingSections) ->
  Pattern = "{\"status\": \"ok\", \"response\": {\"start\": ~1000lp, \"missing\": ~1000p}}",
  AnswerList = io_lib:format(Pattern, [StartNumbers, MissingSections]),
  lists:flatten(AnswerList).

-module(sequence_responses).

-export([
  ok/1
]).


ok(Uuid) ->
  erlang:iolist_to_binary([<<"{\"status\": \"ok\", \"response\": {\"sequence\": \"">>, Uuid, <<"\"}}">>]).

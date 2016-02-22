-module(responses).

-export([
  error/2,
  error/1
]).

error(Msg, Data) ->
  error_logger:info_msg("Data: ~p~n", [Data]),
  responses:error(Msg).

error(Msg) ->
  iolist_to_binary([<<"{\"status\": \"error\", \"msg\": \"">>, Msg, <<"\"}">>]).

-module(responses).

-export([
  error/1
]).

error(Msg) ->
  erlang:iolist_to_binary([<<"{\"status\": \"error\", \"msg\": \"">>, Msg, <<"\"}">>]).

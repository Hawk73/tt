-module(observation_processor).

-include("recdef.hrl").

-export([perform/1]).


perform(_Data = #indication{color=Color, numbers=Numbers, sequence=Sequence}) ->
  error_logger:info_msg("Color: ~p~n", [Color]),
  error_logger:info_msg("Numbers: ~p~n", [Numbers]),
  error_logger:info_msg("Sequence: ~p~n", [Sequence]),
  [[1, 2], ["0000000", "1000010"]].
%%  ok or error


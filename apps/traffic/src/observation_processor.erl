-module(observation_processor).

-include("recdef.hrl").

-export([perform/1]).


perform(_Data = #indication{color=Color, numbers=Numbers, uuid=Uuid}) ->
  error_logger:info_msg("Color: ~p~n", [Color]),
  error_logger:info_msg("Numbers: ~p~n", [Numbers]),
  error_logger:info_msg("Uuid: ~p~n", [Uuid]),
  case sequence_processor:exists_uuid(Uuid) of
    true ->
      sequence_processor:append_data(Numbers, Uuid),
      Finished = Color == <<"red">>,
      find_start_and_missing(Uuid, Finished);
    _ ->
      {error, <<"The sequence isn't found">>}
  end.


find_start_and_missing(Uuid, _Finished) ->
  error_logger:info_msg("Data: ~p~n", [sequence_processor:data(Uuid)]),
  {ok, [[1, 2], ["0000000", "1000010"]]}.

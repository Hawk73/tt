-module(observation_processor).

-export([
  perform/1
]).

-include("recdef.hrl").
-include("numbers.hrl").

perform(_Data = #indication{color=Color, first_e_digit=FirstEDigit, second_e_digit=SecondEDigit, uuid=Uuid}) ->
  error_logger:info_msg("Color: ~p~n", [Color]),
  error_logger:info_msg("First number: ~p~n", [FirstEDigit]),
  error_logger:info_msg("Second number: ~p~n", [SecondEDigit]),
  error_logger:info_msg("Uuid: ~p~n", [Uuid]),
  case sequence_processor:exists_uuid(Uuid) of
    true ->
      sequence_processor:append_data(Uuid, FirstEDigit, SecondEDigit),
      Finished = Color == <<"red">>,
      find_start_and_missing(Uuid, Finished);
    _ ->
      {error, <<"The sequence isn't found.">>}
  end;
perform(_) ->
  {error, <<"Invalid data.">>}.


%% @todo: добавить обработку красного цвета
find_start_and_missing(Uuid, _Finished) ->
  Indications = sequence_processor:data(Uuid),

  case start_finder:perform(Indications) of
    {ok, StartNumbers} ->
      case missing_finder:perform(Indications, StartNumbers) of
        {ok, MissingSections} -> {ok, [StartNumbers, MissingSections]};
        Error -> Error
      end;
    Error -> Error
  end.

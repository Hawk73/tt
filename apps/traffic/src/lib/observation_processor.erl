-module(observation_processor).

-export([
  perform/1
]).

-include("recdef.hrl").
-include("numbers.hrl").

perform(IndicationData = #indication{uuid=Uuid, color=Color, first_e_digit=_, second_e_digit=_}) ->
  case sequence_processor:last_item(Uuid) of
    {Uuid, empty} ->
      case Color of
        green ->
          sequence_processor:delete(Uuid),
          sequence_processor:append_data(IndicationData),
          find_start_and_missing(Uuid, Color);
        _ ->
          errors:no_data()
      end;
    {Uuid, _, _} ->
      sequence_processor:append_data(IndicationData),
      find_start_and_missing(Uuid, Color);
    {Uuid, finished} ->
      errors:data_after_red();
    _ ->
      errors:no_sequence()
  end;
perform(_) -> errors:invalid_data().


find_start_and_missing(Uuid, LastColor) ->
  Indications = sequence_processor:data(Uuid),

  StartData = case LastColor == red of
    true -> {ok, [length(Indications) - 1]};
    _ -> start_finder:perform(Indications)
  end,

  case StartData of
    {ok, StartNumbers} ->
      case missing_finder:perform(Indications, StartNumbers) of
        {ok, MissingSections} -> {ok, [StartNumbers, MissingSections]};
        Error -> Error
      end;
    Error -> Error
  end.

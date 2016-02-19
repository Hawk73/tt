-module(observation_processor).

-export([
  perform/1,
  find_start/1
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

%% Две функции, когда финиш и когда нет
find_start_and_missing(Uuid, _Finished) ->
  Data = sequence_processor:data(Uuid),
  case find_start(Data) of
    {ok, StartNumbers} ->
      %% @todo: найти сломанные секции
      {ok, [StartNumbers, ["0000000", "1000010"]]};
    Error -> Error
  end.


%% Поиск возможных стартовых значений.
%%
find_start(DataArray) ->
  error_logger:info_msg("Data: ~p~n", DataArray),
  case determine_start_number(DataArray, ?POSSIBLE_FIRST_NUMBERS, ?POSSIBLE_SECOND_NUMBERS, 0) of
    {ok, Step, PossibleNumbers} -> {ok, [X+Step || X <- PossibleNumbers]};
    Error -> Error
  end.


%% Поиск стартового числа.
%% Параметры:
%% FirstEDigit - полученное значение на первом циферблате в виде числа, например, 2#1110111=199 - цифра 0,
%% SecondEDigit - полученное значение на втором циферблате.
%% Step - шаг итерации, с помощью него можно будет определить начальное число или их варианты.
determine_start_number(_, [], _, _) -> no_solutions_found();
determine_start_number(_, _, [], _) -> no_solutions_found();

determine_start_number(_, [FirstEDigit], [SecondEDigit], Step) ->
  [FirstDigit, SecondDigit] = numbers:decode_digits([FirstEDigit, SecondEDigit]),
  {ok, Step, [FirstDigit * 10 + SecondDigit]};

determine_start_number([], PossibleFirstEDigits, PossibleSecondEDigits, Step) ->
  {ok, Step, possible_numbers_for(PossibleFirstEDigits, PossibleSecondEDigits)};

determine_start_number([{_Uuid, FirstEDigit, SecondEDigit}|Items], PossibleFirstEDigits, PossibleSecondEDigits, Step) ->
  io:format("----- STEP ~p -----~n", [Step]),
  io:format("FirstEDigit ~p~n", [FirstEDigit]),
  io:format("SecondEDigit ~p~n", [SecondEDigit]),
  io:format("PossibleFirstEDigits ~p~n", [PossibleFirstEDigits]),
  io:format("PossibleSecondEDigits ~p~n", [PossibleSecondEDigits]),
  %% Сужаем возможные значения в зависимости от полученного значения
  SuitableFirstEDigits = suitable_e_digits_for(FirstEDigit, PossibleFirstEDigits, []),
  SuitableSecondEDigits = suitable_e_digits_for(SecondEDigit, PossibleSecondEDigits, []),
  io:format("SuitableFirstEDigits ~p~n", [SuitableFirstEDigits]),
  io:format("SuitableSecondEDigits ~p~n", [SuitableSecondEDigits]),
  io:format("Items ~p~n", [Items]),

  case Items of
    [] ->
      determine_start_number(Items, SuitableFirstEDigits, SuitableSecondEDigits, Step);
    _ ->
      %% Определяем возможные значения для следующего шага
      PossibleNumbers = possible_numbers_for(SuitableFirstEDigits, SuitableSecondEDigits),
      %% Уменьшаем следующие возможные значения на 1, оставляя все что больше 0.
      io:format("PossibleNumbers ~p~n", [PossibleNumbers]),
      NextPossibleNumbers = [X-1 || X <- PossibleNumbers, X-1 > 0],
      io:format("NextPossibleNumbers ~p~n", [NextPossibleNumbers]),
      io:format("possible_digits_for(NextPossibleNumbers) ~p~n", [possible_digits_for(NextPossibleNumbers)]),
      [NextPossibleFirstDigits, NextPossibleSecondDigits] = possible_digits_for(NextPossibleNumbers),
      determine_start_number(Items, NextPossibleFirstDigits, NextPossibleSecondDigits, Step + 1)
  end;

%% Первое значение
determine_start_number([{_Uuid, []}|Items], PossibleFirstEDigits, PossibleSecondEDigits, Step) ->
  determine_start_number(Items, PossibleFirstEDigits, PossibleSecondEDigits, Step);

determine_start_number(_, _, _, _) -> {error, <<"Internal error: code 3.">>}.


%% Возвращает возможные варианты цифр (для одного разряда)
%% Example:
%% SrcDigit 2#1_1_1_0_1_1_1=55 - полученное значение наблюдателем
%% PossibleDigits [119, ..] - возможные значения
suitable_e_digits_for(_, [], Acc) -> Acc;

suitable_e_digits_for(SrcEDigit, [EDigit|PossibleEDigits], Acc) when (SrcEDigit band EDigit) == SrcEDigit ->
  suitable_e_digits_for(SrcEDigit, PossibleEDigits, [EDigit|Acc]);

suitable_e_digits_for(SrcEDigit, [_|PossibleEDigits], Acc) ->
  suitable_e_digits_for(SrcEDigit, PossibleEDigits, Acc).


%% Возможные варианты числа в зависимости от возможных вариантов разрядов.
%%
possible_numbers_for(PossibleFirstEDigits, PossibleSecondEDigits) ->
  [ numbers:decode_digit(X) * 10 + numbers:decode_digit(Y) || X <- PossibleFirstEDigits, Y <- PossibleSecondEDigits].


%% Возможные варианты цифры (разряды) в зависимости от возможных вариантов числа.
%%
possible_digits_for(PossibleNumbers) ->
  %% @todo: может можно сделать за раз ?
  SrcPossibleFirstDigits = [ X div 10 || X <- PossibleNumbers],
  PossibleFirstDigits = non_duplicate_list(SrcPossibleFirstDigits),

  SrcPossibleSecondDigits = [ X rem 10 || X <- PossibleNumbers],
  PossibleSecondDigits = non_duplicate_list(SrcPossibleSecondDigits),

  [numbers:encode_digits(PossibleFirstDigits), numbers:encode_digits(PossibleSecondDigits)].


non_duplicate_list(List) ->
  Set = sets:from_list(List),
  sets:to_list(Set).


no_solutions_found() ->
  {error, <<"No solutions found.">>}.

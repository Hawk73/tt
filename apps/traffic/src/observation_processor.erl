-module(observation_processor).

-export([perform/1]).
-export([run/1]).

-include("recdef.hrl").
-include("numbers.hrl").

perform(_Data = #indication{color=Color, first_number=FirstDigit, second_number=SecondDigit, uuid=Uuid}) ->
  error_logger:info_msg("Color: ~p~n", [Color]),
  error_logger:info_msg("First number: ~p~n", [FirstDigit]),
  error_logger:info_msg("Second number: ~p~n", [SecondDigit]),
  error_logger:info_msg("Uuid: ~p~n", [Uuid]),
  case sequence_processor:exists_uuid(Uuid) of
    true ->
      sequence_processor:append_data(Uuid, FirstDigit, SecondDigit),
      Finished = Color == <<"red">>,
      find_start_and_missing(Uuid, Finished);
    _ ->
      {error, <<"The sequence isn't found.">>}
  end;
perform(_) ->
  {error, <<"Invalid data">>}.

%% Две функции, когда финиш и когда нет
find_start_and_missing(Uuid, _Finished) ->
  error_logger:info_msg("Data: ~p~n", [sequence_processor:data(Uuid)]),
  %% TODO: когда узнаем число, сможем найти сломанные секции еще раз пробежавшись
  {ok, [[1, 2], ["0000000", "1000010"]]}.


%% Стартовые возможные значения:
%% StartNumbers 1..99
%% FirstDigit 0..9
%% SecondDigit 1..9
run(DataArray) ->
  case find_start_number(DataArray, ?POSSIBLE_FIRST_NUMBERS, ?POSSIBLE_SECOND_NUMBERS, 0) of
    {ok, Step, PossibleNumbers} -> [X-Step || X <- PossibleNumbers];
    Error -> Error
  end.


%% Поиск стартового числа.
%% Параметры:
%% FirstDigit - полученное значение на первом циферблате в виде числа, например, 2#1110111=199 - цифра 0,
%% SecondDigit - полученное значение на втором циферблате.
find_start_number(_, [], _, _) -> no_solutions_found();
find_start_number(_, _, [], _) -> no_solutions_found();

find_start_number(_, [FirstDigit], [SecondDigit], Step) ->
  {ok, Step, [FirstDigit * 10 + SecondDigit]};

find_start_number([], PossibleFirstDigits, PossibleSecondDigits, Step) ->
  {ok, Step, possible_numbers_for(PossibleFirstDigits, PossibleSecondDigits)};

find_start_number([{_Uuid, FirstDigit, SecondDigit}|Items], PossibleFirstDigits, PossibleSecondDigits, Step) ->
  %% Сужаем возможные значения в зависимости от полученного значения
  SuitableFirstDigits = suitable_digits(FirstDigit, PossibleFirstDigits, []),
  SuitableSecondDigits = suitable_digits(SecondDigit, PossibleSecondDigits, []),
  io:format("SuitableFirstDigits ~p~n", [SuitableFirstDigits]),
  io:format("SuitableSecondDigits ~p~n", [SuitableSecondDigits]),
  io:format("Items ~p~n", [Items]),

  case Items of
    [] ->
      find_start_number(Items, SuitableFirstDigits, SuitableSecondDigits, Step);
    _ ->
      %% Определяем возможные значения для следующего шага
      PossibleNumbers = possible_numbers_for(PossibleFirstDigits, PossibleSecondDigits),
      %% Уменьшаем следующие возможные значения на 1, оставляя все что больше 0.
      NextPossibleNumbers = [X-1 || X <- PossibleNumbers, X-1 > 0],
      [NextPossibleFirstDigits, NextPossibleSecondDigits] = possible_digits_for(NextPossibleNumbers),
      find_start_number(Items, NextPossibleFirstDigits, NextPossibleSecondDigits, Step + 1)
  end;

find_start_number(_, _, _, _) -> {error, <<"Internal error.">>}.


%% Возвращает возможные варианты цифр (для одного разряда)
%% Example:
%% SrcNumber 2#1_1_1_0_1_1_1=55 - полученное значение наблюдателем
%% PossibleDigits [119, ..] - возможные значения
suitable_digits(_, [], Acc) -> Acc;

suitable_digits(SrcNumber, [Number|PossibleDigits], Acc) when (SrcNumber band Number) == SrcNumber ->
  suitable_digits(SrcNumber, PossibleDigits, [Number|Acc]);

suitable_digits(SrcNumber, [_|PossibleDigits], Acc) ->
  suitable_digits(SrcNumber, PossibleDigits, Acc).


%% Возможные варианты числа в зависимости от возможных вариантов разрядов.
%%
possible_numbers_for(PossibleFirstDigits, PossibleSecondDigits) ->
  [ decode_digit(X) * 10 + decode_digit(Y) || X <- PossibleFirstDigits, Y <- PossibleSecondDigits].


%% Возможные варианты цифры (разряды) в зависимости от возможных вариантов числа.
%%
possible_digits_for(PossibleNumbers) ->
  %% @todo: может можно сделать за раз ?
  SrcPossibleFirstDigits = [ X div 10 || X <- PossibleNumbers],
  PossibleFirstDigits = non_duplicate_list(SrcPossibleFirstDigits),

  SrcPossibleSecondDigits = [ X rem 10 || X <- PossibleNumbers],
  PossibleSecondDigits = non_duplicate_list(SrcPossibleSecondDigits),

  {PossibleFirstDigits, PossibleSecondDigits}.

non_duplicate_list(List) ->
  Set = sets:from_list(List),
  sets:to_list(Set).

no_solutions_found() ->
  {error, <<"No solutions found.">>}.

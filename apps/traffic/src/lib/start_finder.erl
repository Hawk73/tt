-module(start_finder).

-include("recdef.hrl").
-include("numbers.hrl").


-export([
  perform/1
]).


%% Поиск возможных стартовых значений.
%%
perform(Indications) ->
  case determine_start_number(Indications, ?POSSIBLE_FIRST_NUMBERS, ?POSSIBLE_SECOND_NUMBERS, 0) of
    {ok, Step, PossibleNumbers} -> {ok, [X+Step || X <- PossibleNumbers]};
    Error -> Error
  end.


%% Поиск стартового числа.
%% Параметры:
%% FirstEDigit - полученное значение на первом циферблате в виде числа, например, 2#1110111=199 - цифра 0,
%% SecondEDigit - полученное значение на втором циферблате.
%% Step - шаг итерации, с помощью него можно будет определить начальное число или их варианты.
determine_start_number(_, [], _, _) -> errors:no_solutions();
determine_start_number(_, _, [], _) -> errors:no_solutions();

determine_start_number(_, [FirstEDigit], [SecondEDigit], Step) ->
  [FirstDigit, SecondDigit] = numbers:decode_digits([FirstEDigit, SecondEDigit]),
  {ok, Step, [FirstDigit * 10 + SecondDigit]};

determine_start_number([], PossibleFirstEDigits, PossibleSecondEDigits, Step) ->
  {ok, Step, possible_numbers_for(PossibleFirstEDigits, PossibleSecondEDigits)};

determine_start_number([{_, FirstEDigit, SecondEDigit}|Items], PossibleFirstEDigits, PossibleSecondEDigits, Step) ->
  %% Сужаем возможные значения в зависимости от полученного значения
  SuitableFirstEDigits = suitable_e_digits_for(FirstEDigit, PossibleFirstEDigits, []),
  SuitableSecondEDigits = suitable_e_digits_for(SecondEDigit, PossibleSecondEDigits, []),

  %% @todo: ускорение поиска ??
  %%  когда Step = 0
  %% 1. получить PossibleNumbers
  %% 2. исключить варианты у которые не хватает секунд для всех итераций

  case Items of
    [] ->
      determine_start_number(Items, SuitableFirstEDigits, SuitableSecondEDigits, Step);
    _ ->
      %% Определяем возможные значения для следующего шага
      PossibleNumbers = possible_numbers_for(SuitableFirstEDigits, SuitableSecondEDigits),
      %% Уменьшаем следующие возможные значения на 1, оставляя все что больше 0.
      NextPossibleNumbers = [X-1 || X <- PossibleNumbers, X-1 > 0],
      [NextPossibleFirstEDigits, NextPossibleSecondEDigits] = numbers:possible_e_digits_for(NextPossibleNumbers),
      determine_start_number(Items, NextPossibleFirstEDigits, NextPossibleSecondEDigits, Step + 1)
  end;

determine_start_number(_, _, _, _) -> {error, <<"Internal error: code 3">>}.


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

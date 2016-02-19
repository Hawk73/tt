-module(missing_finder).

-include("recdef.hrl").
-include("numbers.hrl").


-export([
  perform/2
]).


perform(Indications, StartNumbers) ->
  case perform(Indications, StartNumbers, [0, 0]) of
    {ok, MissingEDigits} -> {ok, numbers:e_digits_to_strings(MissingEDigits)};
    Error -> Error
  end.


perform([], _, [FirstMissing, SecondMissing]) -> {ok, [FirstMissing, SecondMissing]};
perform([{_Uuid, FirstEDigit, SecondEDigit}|Indications], CurrentNumbers, [FirstMissing, SecondMissing]) ->
  [PossibleFirstEDigits, PossibleSecondEDigits] = numbers:possible_e_digits_for(CurrentNumbers),

  ExpectedFirstEDigit = mutual_e_digit(PossibleFirstEDigits),
  CurrentFirstMissing = FirstEDigit bxor ExpectedFirstEDigit,

  ExpectedSecondEDigit = mutual_e_digit(PossibleSecondEDigits),
  CurrentSecondMissing = SecondEDigit bxor ExpectedSecondEDigit,

  NextNumbers = [X-1 || X <- CurrentNumbers, X-1 > 0],
  perform(Indications, NextNumbers, [FirstMissing bor CurrentFirstMissing, SecondMissing bor CurrentSecondMissing]);

%% Первое значение
perform([{_Uuid, []}|Indications], CurrentNumbers, [FirstMissing, SecondMissing]) ->
  perform(Indications, CurrentNumbers, [FirstMissing, SecondMissing]).


%% Находит точно горящие секции.
mutual_e_digit(PossibleEDigits) ->
  mutual_e_digit(PossibleEDigits, ?EIGHT).

mutual_e_digit([], EDigit) -> EDigit;
mutual_e_digit([PossibleEDigit|PossibleEDigits], EDigit) -> mutual_e_digit(PossibleEDigits, EDigit band PossibleEDigit).

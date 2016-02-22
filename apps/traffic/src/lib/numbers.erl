-module(numbers).

-include("numbers.hrl").

-export([
  parse_binary_digits/1,
  e_digits_to_strings/1,
  encode_digits/1,
  encode_digit/1,
  decode_digits/1,
  decode_digit/1,
  possible_e_digits_for/1
]).


parse_binary_digits(BinaryStrings) -> [ parse_binary_digit(X) || X <- BinaryStrings].

%% Example BinaryString <<"1110111">>
%% @todo: проверить BinaryString
parse_binary_digit(BinaryString) ->
  %% Example: [1,1,1,0,1,1,1]
  List = [ binary_to_integer(X) || <<X:1/binary>> <= BinaryString],
  list_to_e_digit(List, 0).

list_to_e_digit(Bits, _) when length(Bits) > 7 -> {error};
list_to_e_digit([Bit|_], _) when Bit > 1 -> {error};
list_to_e_digit([], Acc) -> {ok, Acc};
list_to_e_digit([Bit|Bits], Acc) when Bit == 0 -> list_to_e_digit(Bits, Acc);
list_to_e_digit([Bit|Bits], Acc) when Bit == 1 ->
  Weight = length(Bits) + 1,
  list_to_e_digit(Bits, Acc + element(Weight, ?BINARY_WEIGHT));
list_to_e_digit(_, _) -> {error}.



%% Пример: [0, 64] -> ["0000000", "1000000"].
e_digits_to_strings(EDigits) -> [ e_digit_to_string(X) || X <- EDigits].

e_digit_to_string(EDigit) ->
  DigitString = e_digit_to_string(EDigit + 128, ""),
  string:substr(DigitString, 2).

e_digit_to_string(0, Acc) -> Acc;
e_digit_to_string(EDigit, Acc) ->
  Bit = EDigit rem 2,
  BitStr = integer_to_list(Bit),
  e_digit_to_string(EDigit div 2, BitStr ++ Acc).



encode_digits(Digits) ->
 [encode_digit(X) || X <- Digits].


encode_digit(Digit) ->
  case Digit of
    0 -> ?ZERO;
    1 -> ?ONE;
    2 -> ?TWO;
    3 -> ?THREE;
    4 -> ?FOUR;
    5 -> ?FIVE;
    6 -> ?SIX;
    7 -> ?SEVEN;
    8 -> ?EIGHT;
    9 -> ?NINE
  end.


decode_digits(EDigits) ->
  [decode_digit(X) || X <- EDigits].


decode_digit(EDigit) ->
  case EDigit of
    ?ZERO -> 0;
    ?ONE -> 1;
    ?TWO -> 2;
    ?THREE -> 3;
    ?FOUR -> 4;
    ?FIVE -> 5;
    ?SIX -> 6;
    ?SEVEN -> 7;
    ?EIGHT -> 8;
    ?NINE -> 9
  end.


%% Возможные варианты цифр (разрядов) в зависимости от возможных вариантов числа.
%% Пример: [02, 08, 82, 88] -> [[0, 8], [2, 8]] -> [[119, 127], [93, 127]].
%%
possible_e_digits_for(PossibleNumbers) ->
  %% @todo: может можно сделать за раз ?
  SrcPossibleFirstDigits = [ X div 10 || X <- PossibleNumbers],
  PossibleFirstDigits = non_duplicate_list(SrcPossibleFirstDigits),

  SrcPossibleSecondDigits = [ X rem 10 || X <- PossibleNumbers],
  PossibleSecondDigits = non_duplicate_list(SrcPossibleSecondDigits),

  [numbers:encode_digits(PossibleFirstDigits), numbers:encode_digits(PossibleSecondDigits)].


non_duplicate_list(List) ->
  Set = sets:from_list(List),
  sets:to_list(Set).

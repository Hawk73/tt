-module(numbers).

-include("numbers.hrl").

-export([
  parse_binary_digits/1,
  encode_digits/1,
  encode_digit/1,
  decode_digits/1,
  decode_digit/1
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

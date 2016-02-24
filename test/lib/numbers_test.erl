-module(numbers_test).

-include_lib("eunit/include/eunit.hrl").

-include("numbers.hrl").

e_digits_to_strings_test() ->
  ?assertEqual(
    ["0000000", "0000001", "0100001", "1000000"],
    numbers:e_digits_to_strings([0, 1, 2#0100001, 2#1000000])
  ).


encode_digits_test() ->
  [
    ?assertEqual(
      [?ZERO, ?ONE, ?TWO, ?THREE, ?FOUR, ?FIVE, ?SIX, ?SEVEN, ?EIGHT, ?NINE],
      numbers:encode_digits([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    ),
    ?assertEqual(
      [?SEVEN, ?NINE],
      numbers:encode_digits([7, 9])
    )
  ].

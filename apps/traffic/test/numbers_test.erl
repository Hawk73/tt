-module(numbers_test).

-include_lib("eunit/include/eunit.hrl").


e_digits_to_strings_test() ->
  ?assertEqual(
    ["0000000", "0000001", "0100001", "1000000"],
    numbers:e_digits_to_strings([0, 1, 2#0100001, 2#1000000])
  ).

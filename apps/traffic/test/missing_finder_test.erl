-module(missing_finder_test).

-include_lib("eunit/include/eunit.hrl").

perform_test() ->
  Uuid = 1,
  ?assertEqual(
    {ok, ["0000000", "1000000"]},
    missing_finder:perform(
      [{Uuid, 2#1110111, 2#0011101}],
      [2, 8, 82, 88]
    )
  ),
  ?assertEqual(
    {ok, ["0000000", "1000010"]},
    missing_finder:perform(
      [{Uuid, 2#1110111, 2#0011101}, {Uuid, 2#1110111, 2#0010000}],
      [2, 8, 82, 88]
    )
  ).

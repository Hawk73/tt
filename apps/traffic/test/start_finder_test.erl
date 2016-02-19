-module(start_finder_test).

-include_lib("eunit/include/eunit.hrl").

find_start_test() ->
  [
    ?assertEqual({ok, [88,82,8,2]}, start_finder:perform(
      [{1, 2#1110111, 2#0011101}]
    )),

    ?assertEqual({ok, [88,82,8,2]}, start_finder:perform([
      {1, 2#1110111, 2#0011101},
      {1, 2#1110111, 2#0010000}
    ]))
  ].

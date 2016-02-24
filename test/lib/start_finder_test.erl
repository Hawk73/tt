-module(start_finder_test).

-include_lib("eunit/include/eunit.hrl").

-include("recdef.hrl").
-include("numbers.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
-define(TEST_UUID, <<"Test uuid">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

find_start_test_() ->
  [
    {"Test from doc", ?setup(fun example_test/0)},
    {"Returns no solution error", ?setup(fun no_solution_error_test/0)},
    {"All sections are broken", ?setup(fun all_broken_test/0)},
    {"Witout broken sections", ?setup(fun without_broken/0)},
    {"Other tests", ?setup(fun other_test/0)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  ok.

stop(_) ->
  ok.

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

example_test() ->
  [
    ?assertEqual({ok, [88, 82, 8, 2]}, start_finder:perform([
      {?TEST_UUID, 2#1110111, 2#0011101}
    ])),

    ?assertEqual({ok, [88, 82, 8, 2]}, start_finder:perform([
      {?TEST_UUID, 2#1110111, 2#0011101},
      {?TEST_UUID, 2#1110111, 2#0010000}
    ]))
  ].

no_solution_error_test() ->
  [
    %% 02 -> 01 -> 02
    ?assertEqual({error, <<"No solutions found">>}, start_finder:perform([
      {?TEST_UUID, ?ZERO, ?TWO},
      {?TEST_UUID, ?ZERO, ?ONE},
      {?TEST_UUID, ?ZERO, ?TWO}
    ]))
  ].

all_broken_test() ->
  Digits = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
  ExpectedStartNumbers1 = [ X * 10 + Y || X <- Digits, Y <- Digits ] -- [0],
  ExpectedStartNumbers2 = ExpectedStartNumbers1 -- [1],

  [
    ?assertEqual({ok, ExpectedStartNumbers1}, start_finder:perform([
      {?TEST_UUID, 0, 0}
    ])),
    ?assertEqual({ok, ExpectedStartNumbers2}, start_finder:perform([
      {?TEST_UUID, 0, 0}, {?TEST_UUID, 0, 0}
    ]))
  ].

without_broken() ->
  [
    ?assertEqual({ok, [83]}, start_finder:perform([
      {?TEST_UUID, ?EIGHT, ?THREE},
      {?TEST_UUID, ?EIGHT, ?TWO},
      {?TEST_UUID, ?EIGHT, ?ONE},
      {?TEST_UUID, ?EIGHT, ?ZERO}
    ]))
  ].

other_test() ->
  [
    %% 15, сломанные секции 1-я цифра: 5; 2-я цифра: 3
    ?assertEqual({ok, [95,85,75,45,35,25,15,5]}, start_finder:perform([
      {?TEST_UUID, 2#0010000, 2#1100011},
      {?TEST_UUID, 2#0010000, 2#0110010},
      {?TEST_UUID, 2#0010000, 2#1010011},
      {?TEST_UUID, 2#0010000, 2#1010101}
    ])),

    %% 10, сломанные секции 1-я цифра: 2 и 5; 2-я цифра: -
    ?assertEqual({ok, [90,70,10]}, start_finder:perform([
      {?TEST_UUID, 2#0000000, ?ZERO},
      {?TEST_UUID, 2#1100101, ?NINE}
    ]))
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

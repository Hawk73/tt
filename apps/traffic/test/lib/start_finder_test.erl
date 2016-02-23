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

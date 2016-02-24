-module(missing_finder_test).

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
    {"All sections are broken", ?setup(fun all_broken_test/0)},
    {"Witout broken sections", ?setup(fun without_broken/0)},
    {"Test from doc", ?setup(fun example_test/0)}
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

all_broken_test() ->
  Digits = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
  StartNumbers = [ X * 10 + Y || X <- Digits, Y <- Digits ],
  [
    ?assertEqual({ok, ["0000000", "0000000"]}, missing_finder:perform(
      [{?TEST_UUID, 0, 0}],
      StartNumbers
    )),
    ?assertEqual({ok, ["0000000", "0000000"]}, missing_finder:perform(
      [{?TEST_UUID, 0, 0}, {?TEST_UUID, 0, 0}],
      StartNumbers
    ))
  ].

without_broken() ->
  [
    ?assertEqual({ok, ["0000000", "0000000"]}, missing_finder:perform(
      [{?TEST_UUID, ?EIGHT, ?THREE},
        {?TEST_UUID, ?EIGHT, ?TWO},
        {?TEST_UUID, ?EIGHT, ?ONE},
        {?TEST_UUID, ?EIGHT, ?ZERO}],
      [83]
    ))
  ].

example_test() ->
  [
    ?assertEqual({ok, ["0000000", "1000000"]}, missing_finder:perform(
      [{?TEST_UUID, 2#1110111, 2#0011101}],
      [2, 8, 82, 88]
    )),
    ?assertEqual({ok, ["0000000", "1000010"]}, missing_finder:perform(
      [{?TEST_UUID, 2#1110111, 2#0011101}, {?TEST_UUID, 2#1110111, 2#0010000}],
      [2, 8, 82, 88]
    ))
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

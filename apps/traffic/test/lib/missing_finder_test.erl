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

example_test() ->
  [
    ?assertEqual(
      {ok, ["0000000", "1000000"]},
      missing_finder:perform(
        [{?TEST_UUID, 2#1110111, 2#0011101}],
        [2, 8, 82, 88]
      )
    ),
    ?assertEqual(
      {ok, ["0000000", "1000010"]},
      missing_finder:perform(
        [{?TEST_UUID, 2#1110111, 2#0011101}, {?TEST_UUID, 2#1110111, 2#0010000}],
        [2, 8, 82, 88]
      )
    )
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

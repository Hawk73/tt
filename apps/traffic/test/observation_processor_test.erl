-module(observation_processor_test).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

find_start_test() ->
  [
    ?assertEqual({ok, [88,82,8,2]}, observation_processor:find_start(
      [{1, 2#1110111, 2#0011101}]
    )),

    ?assertEqual({ok, [88,82,8,2]}, observation_processor:find_start([
      {1, 2#1110111, 2#0011101},
      {1, 2#1110111, 2#0010000}
    ]))
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%



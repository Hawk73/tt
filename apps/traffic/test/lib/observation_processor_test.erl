-module(observation_processor_test).

-include_lib("eunit/include/eunit.hrl").

-include("recdef.hrl").
-include("numbers.hrl").

-define(TEST_UUID, <<"Test uuid">>).
-define(setup(F), {setup, fun start/0, fun stop/1, F}).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

perform_test_() ->
  [
    {"Returns error when uuid did not find", ?setup(fun invalid_uuid/0)},
    {"Returns error when first data is red", ?setup(fun first_red/0)},
    {"Returns error when red has been already", ?setup(fun data_after_red/0)},
    {"Deletes first flag when first indication added", ?setup(fun delete_first/0)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%

start() ->
  sequence_processor:start().

stop(_) ->
  sequence_processor:stop().

%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

invalid_uuid() ->
  [
    ?assertEqual(
      {error, <<"The sequence isn't found">>},
      observation_processor:perform(
        #indication{uuid = <<"invlaid_uuid">>, color = <<"green">>, first_e_digit = ?ZERO, second_e_digit = ?ZERO}
      )
    )
  ].

first_red() ->
  {Uuid, true} = sequence_processor:create_uuid(),
  [
    ?assertEqual(
      {error, <<"There isn't enough data">>},
      observation_processor:perform(
        #indication{uuid = Uuid, color = <<"red">>, first_e_digit = ?ZERO, second_e_digit = ?ZERO}
      )
    )
  ].

data_after_red() ->
  {Uuid, true} = sequence_processor:create_uuid(),
  observation_processor:perform(
    #indication{uuid = Uuid, color = <<"green">>, first_e_digit = ?ZERO, second_e_digit = ?ZERO}
  ),
  observation_processor:perform(
    #indication{uuid = Uuid, color = <<"red">>}
  ),
  [
    ?assertEqual(
      {error, <<"The red observation should be the last">>},
      observation_processor:perform(
        #indication{uuid = Uuid, color = <<"green">>, first_e_digit = ?ZERO, second_e_digit = ?ZERO}
      )
    ),
    ?assertEqual(
      {error, <<"The red observation should be the last">>},
      observation_processor:perform(
        #indication{uuid = Uuid, color = <<"red">>, first_e_digit = ?ZERO, second_e_digit = ?ZERO}
      )
    )
  ].

delete_first() ->
  {Uuid, true} = sequence_processor:create_uuid(),
  observation_processor:perform(
    #indication{uuid = Uuid, color = <<"green">>, first_e_digit = ?ONE, second_e_digit = ?TWO}
  ),
  Data = sequence_processor:data(Uuid),
  [
    ?assertEqual(1, length(Data)),
    ?assertEqual({Uuid, ?ONE, ?TWO}, lists:last(Data))
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

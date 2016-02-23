-module(clear_handler_test).

-include_lib("eunit/include/eunit.hrl").

-include("recdef.hrl").
-include("numbers.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_test_() ->
  [
    {"Clears all data", ?setup(fun clear_data/0)}
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

clear_data() ->
  {Uuid, true} = sequence_processor:create_uuid(),
  observation_processor:perform(
    #indication{uuid = Uuid, color = green, first_e_digit = ?ZERO, second_e_digit = ?ZERO}
  ),
  stub_cowboy_reply(),
  clear_handler:handle({}, {}),
  [
    ?assertEqual(
      '$end_of_table',
      ets:last(?TAB)
    )
  ].

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

stub_cowboy_reply() ->
  meck:expect(cowboy_req, reply, fun(_, _, _, _) -> {ok, ""} end).

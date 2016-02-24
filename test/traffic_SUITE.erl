-module(traffic_SUITE).

%%-compile(export_all).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
  not_found_test_case/1,
  create_sequence_test_case/1,
  add_observation_test_case/1,
  clear_test_case/1,
  example_test_case/1
]).


-include_lib("common_test/include/ct.hrl").

% etest macros
-include_lib ("etest/include/etest.hrl").
% etest_http macros
-include_lib ("etest_http/include/etest_http.hrl").


all() ->
  [
    not_found_test_case,
    create_sequence_test_case,
    add_observation_test_case,
    clear_test_case,
    example_test_case
  ].


not_found_test_case(_Config) ->
  Response = ?perform_get("http://localhost:8080/"),
  ?assert_status(404, Response),
  ?assert_body("404 Page Not Found", Response).


create_sequence_test_case(_Config) ->
  Response = ?perform_get("http://localhost:8080/sequence/create"),
  ?assert_status(200, Response),
  ?assert_body_contains("{\"status\": \"ok\", \"response\": {\"sequence\": \"", Response).


add_observation_test_case(_Config) ->
  RequestHeaders =  "",
  RequestBody =  "{\"observation\": {\"color\": \"red\"}, \"sequence\": \"b839e67c-d637-4afc-9241-63943c4fea83\"}",
  Response = ?perform_post("http://localhost:8080/observation/add", RequestHeaders, RequestBody),
  ?assert_status(200, Response),
  ?assert_body("{\"status\": \"error\", \"msg\": \"The sequence isn't found\"}", Response).


clear_test_case(_Config) ->
  Response = ?perform_post("http://localhost:8080/clear"),
  ?assert_status(200, Response),
  ?assert_body("{\"status\": \"ok\", \"response\": \"ok\"}", Response).


example_test_case(_Config) ->
  ?perform_post("http://localhost:8080/clear"),

  FakeUuid = "fake-uuid",
  stub_generate_uuid(FakeUuid),
  ?perform_post("http://localhost:8080/sequence/create"),

  RequestHeaders =  "",

  RequestBody1 =
    "{\"observation\": {\"color\": \"green\",\"numbers\": [\"1110111\", \"0011101\"]},\"sequence\": \"fake-uuid\"}",
  Response1 = ?perform_post("http://localhost:8080/observation/add", RequestHeaders, RequestBody1),
  ?assert_status(200, Response1),
  ?assert_body(
    "{\"status\": \"ok\", \"response\": {\"start\": [88,82,8,2], \"missing\": [\"0000000\",\"1000000\"]}}",
    Response1
  ),

  RequestBody2 =
    "{\"observation\": {\"color\": \"green\",\"numbers\": [\"1110111\", \"0010000\"]},\"sequence\": \"fake-uuid\"}",
  Response2 = ?perform_post("http://localhost:8080/observation/add", RequestHeaders, RequestBody2),
  ?assert_status(200, Response2),
  ?assert_body(
    "{\"status\": \"ok\", \"response\": {\"start\": [88,82,8,2], \"missing\": [\"0000000\",\"1000010\"]}}",
    Response2
  ),

  RequestBody3 = "{\"observation\": {\"color\": \"red\"},\"sequence\": \"fake-uuid\"}",
  Response3 = ?perform_post("http://localhost:8080/observation/add", RequestHeaders, RequestBody3),
  ?assert_status(200, Response3),
  ?assert_body(
    "{\"status\": \"ok\", \"response\": {\"start\": [2], \"missing\": [\"0000000\",\"1000010\"]}}",
    Response3
  ).

%%suite() ->
%%  [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
%%  os:cmd("./_rel/bin/traffic start"),
%%  Config.
  {ok, App_Start_List} = start([traffic]),
  inets:start(),
  [{app_start_list, App_Start_List}|Config].

end_per_suite(Config) ->
%%  os:cmd("./_rel/bin/traffic stop"),
%%  ok.
  inets:stop(),
  stop(?config(app_start_list, Config)),
  Config.

%%init_per_testcase(_TestCase, Config) ->
%%  Config.
%%
%%end_per_testcase(_TestCase, _Config) ->
%%  ok.


start(Apps) -> {ok, do_start(_To_start = Apps, _Started = [])}.

do_start([], Started) -> Started;
do_start([App|Apps], Started) ->
  case application:start(App) of
    ok ->
      do_start(Apps, [App|Started]);
    {error, {not_started, Dep}} ->
      do_start([Dep|[App|Apps]], Started)
  end.

stop(Apps) ->
  _ = [ application:stop(App) || App <- Apps ],
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

stub_generate_uuid(FakeUuid) ->
  meck:expect(uuid, uuid_to_string, fun(_) -> FakeUuid end).

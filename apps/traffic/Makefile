PROJECT = traffic
PROJECT_DESCRIPTION = Traffic App
PROJECT_VERSION = 1.0.0

DEPS = cowboy uuid jsx

TEST_DEPS = etest_http

include erlang.mk

test: tests

on:
	@( ./_rel/traffic_release/bin/traffic_release start )

off:
	@( ./_rel/traffic_release/bin/traffic_release stop )

reon: off on

console:
	@( erl -pa ebin deps/*/ebin )

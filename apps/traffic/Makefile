PROJECT = traffic
PROJECT_DESCRIPTION = Traffic App
PROJECT_VERSION = 1.0.0

DEPS = cowboy uuid jsx

include erlang.mk

on:
	@( ./_rel/traffic_release/bin/traffic_release start )

off:
	@( ./_rel/traffic_release/bin/traffic_release stop )

reon: off on

console:
	@( erl -pa ebin deps/*/ebin )

REBAR = `which rebar`

all: deps compile generate

deps:
	@( $(REBAR) get-deps )

compile: clean
	@( $(REBAR) compile )

generate: generate
	@( $(REBAR) generate )

clean:
	@( $(REBAR) clean )

run:
	@( erl -pa ebin deps/*/ebin -s traffic )

start:
	@( ./rel/traffic_project/bin/traffic_project start )

stop:
	@( ./rel/traffic_project/bin/traffic_project stop )

console:
	@( ./rel/traffic_project/bin/traffic_project console )


.PHONY: all deps compile generate clean run start stop console
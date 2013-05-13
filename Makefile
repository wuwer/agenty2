all: compile

.PHONY: get-rebar get-deps

get-rebar:
	if [ ! -f rebar ]; \
	then \
		wget https://raw.github.com/wiki/rebar/rebar/rebar && \
		chmod u+x rebar; \
	fi;

get-deps: get-rebar
	./rebar get-deps

compile: get-deps
	./rebar compile

create-node: get-deps
	mkdir -p rel
	cd rel && ../rebar create-node nodeid=agenty2013

generate: compile
	./rebar generate

start: generate
	./rel/agenty2013/bin/agenty2013 start

console: generate
	./rel/agenty2013/bin/agenty2013 console

attach:
	./rel/agenty2013/bin/agenty2013 attach

stop:
	./rel/agenty2013/bin/agenty2013 stop

clean:
	./rel/agenty2013/bin/agenty2013 clean

all: get-rebar get-deps

.PHONY: get-rebar get-deps

get-rebar:
	if [ ! -f rebar ]; \
	then \
		wget https://raw.github.com/wiki/rebar/rebar/rebar && \
		chmod u+x rebar; \
	fi;

get-deps: get-rebar
	./rebar get-deps

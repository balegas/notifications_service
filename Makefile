REBAR = $(shell pwd)/rebar3
.PHONY: rel test relgentlerain

all: compile

compile:
	${REBAR} compile

clean:
	${REBAR} clean

dialyzer:
	${REBAR} dialyzer

test:
	mkdir -p logs
	${REBAR} eunit skip_deps=true

coverage:
	${REBAR} cover --verbose

lint:
	${REBAR} as lint lint
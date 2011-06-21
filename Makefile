.PHONY: deps

all: deps compile
	./rebar skip_deps=true escriptize

deps:
	./rebar get-deps

compile: deps
	./rebar compile

clean:
	@./rebar clean

distclean: clean
	@rm -rf timeit deps

test: deps compile
	./rebar skip_deps=true eunit

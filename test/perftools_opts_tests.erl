-module(perftools_opts_tests).
-include_lib("eunit/include/eunit.hrl").

no_args_test() ->
    perftools_opts:parse([]).

name_test() ->
    perftools_opts:parse(["-name", "test@127.0.0.1"]),
    ?assertEqual('test@127.0.0.1', node()).

setcookie_test() ->
    perftools_opts:parse(["-setcookie", "test"]),
    ?assertEqual('test', erlang:get_cookie()).

name_and_setcookie_test() ->
    perftools_opts:parse(["-name", "test@127.0.0.1", "-setcookie", "test"]),
    ?assertEqual('test@127.0.0.1', node()),
    ?assertEqual('test', erlang:get_cookie()).

setcookie_and_name_test() ->
    perftools_opts:parse(["-setcookie", "test", "-name", "test@127.0.0.1"]),
    ?assertEqual('test@127.0.0.1', node()),
    ?assertEqual('test', erlang:get_cookie()).

-module(timeit_tests).
-include_lib("eunit/include/eunit.hrl").

no_args_test() ->
    timeit:main("").

name_test() ->
    timeit:main(["-name", "test@127.0.0.1"]),
    ?assertEqual('test@127.0.0.1', node()).

setcookie_test() ->
    timeit:main(["-setcookie", "test"]),
    ?assertEqual('test', erlang:get_cookie()).

name_and_setcookie_test() ->
    timeit:main(["-name", "test@127.0.0.1", "-setcookie", "test"]),
    ?assertEqual('test@127.0.0.1', node()),
    ?assertEqual('test', erlang:get_cookie()).

setcookie_and_name_test() ->
    timeit:main(["-setcookie", "test", "-name", "test@127.0.0.1"]),
    ?assertEqual('test@127.0.0.1', node()),
    ?assertEqual('test', erlang:get_cookie()).

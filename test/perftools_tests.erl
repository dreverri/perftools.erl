-module(perftools_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(NAME, 'tracer@127.0.0.1').
-define(NODE, 'traced@127.0.0.1').
-define(COOKIE, 'perftools_tests').
-define(TRACE_FILE, "perftools_tests.trace").
-define(PROF_FILE, "perftools_tests.prof").

setup() ->
    os:cmd("rm " ++ ?TRACE_FILE ++ " " ++ ?PROF_FILE),
    net_kernel:start([?NAME, longnames]),
    erlang:set_cookie(node(), ?COOKIE).

teardown(_) ->
    net_kernel:stop(),
    ok.

should_write_trace_file() ->
    ok = perftools:trace([?NODE], [{string, len, '_'}], ?TRACE_FILE),
    5 = rpc:call(?NODE, string, len, ["hello"]),
    ok = perftools:stop(),
    ?assert(filelib:is_file(?TRACE_FILE)).

should_write_prof_file() ->
    ok = perftools:trace([?NODE], [{string, len, '_'}], ?TRACE_FILE),
    5 = rpc:call(?NODE, string, len, ["hello"]),
    ok = perftools:stop(),
    ok = perftools:profile(?TRACE_FILE, ?PROF_FILE),
    ?assert(filelib:is_file(?PROF_FILE)).

trace_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {generator,
      fun() ->
              {node,
               ?NODE,
               "-setcookie " ++ atom_to_list(?COOKIE),
               perftools_tests()
              }
      end}}.

perftools_tests() ->
    Functions = ?MODULE:module_info(functions),
    Filtered = lists:filter(fun filter/1, Functions),
    lists:map(fun convert/1, Filtered).

filter({Function, 0}) ->
    lists:prefix("should_", atom_to_list(Function));

filter(_) ->
    false.

convert({Function, 0}) ->
    fun() -> apply(?MODULE, Function, []) end.

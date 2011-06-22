-module(timeit_server_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(NAME, 'test@127.0.0.1').
-define(NODE, 'one@127.0.0.1').
-define(COOKIE, 'timeit_server_tests').

trace_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {generator,
      fun() ->
              {node,
               ?NODE,
               "-setcookie " ++ atom_to_list(?COOKIE),
               trace_tests()
              }
      end}}.

setup() ->
    os:cmd("rm -r trace_test"),
    net_kernel:start([?NAME, longnames]),
    erlang:set_cookie(node(), ?COOKIE).

teardown(_) ->
    timeit_server:stop_all(),
    net_kernel:stop(),
    ok.

trace_tests() ->
    Functions = ?MODULE:module_info(functions),
    Filtered = lists:filter(fun filter/1, Functions),
    lists:map(fun convert/1, Filtered).

filter({Function, 0}) ->
    lists:prefix("should_", atom_to_list(Function));

filter(_) ->
    false.

convert({Function, 0}) ->
    fun() -> apply(?MODULE, Function, []) end.

should_write_trace_file() ->
    {ok, Pid} = timeit_server:open("trace_test"),
    ok = timeit_server:start_trace(Pid, [?NODE], [{string, len, '_'}]),
    5 = rpc:call(?NODE, string, len, ["hello"]),
    ok = timeit_server:stop_trace(Pid),
    ?assertEqual(1, length(filelib:wildcard("trace_test/trace.*"))).

should_fold_over_trace_data() ->
    {ok, Pid} = timeit_server:open("trace_test"),
    ok = timeit_server:start_trace(Pid, [?NODE], [{string, len, '_'}]),
    5 = rpc:call(?NODE, string, len, ["hello"]),
    ok = timeit_server:stop_trace(Pid),
    Fun = fun(O, A) -> [O|A] end,
    Actual = timeit_server:fold(Pid, Fun, []),
    ?assertEqual(2, length(Actual)).

should_write_a_profile_file() ->
    {ok, Pid} = timeit_server:open("trace_test"),
    ok = timeit_server:start_trace(Pid, [?NODE], [{string, len, '_'}]),
    5 = rpc:call(?NODE, string, len, ["hello"]),
    ok = timeit_server:stop_trace(Pid),
    ok = timeit_server:profile(Pid),
    ?assertEqual(1, length(filelib:wildcard("trace_test/profile.*"))).

should_write_a_profile_file1() ->
    {ok, Pid} = timeit_server:open("trace_test"),
    ok = timeit_server:start_trace(Pid, [?NODE], [{lists, '_', '_'}]),
    1 = rpc:call(?NODE, lists, nth, [1, [1,2,3]]),
    [1, 2, 3] = rpc:call(?NODE, lists, flatten, [[1, [2,3]]]),
    ok = timeit_server:stop_trace(Pid),
    ok = timeit_server:profile(Pid),
    ?assertEqual(1, length(filelib:wildcard("trace_test/profile.*"))).

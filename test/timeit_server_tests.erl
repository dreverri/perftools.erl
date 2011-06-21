-module(timeit_server_tests).
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
    net_kernel:start([?NAME, longnames]),
    erlang:set_cookie(node(), ?COOKIE).

teardown(_) ->
    timeit_server:stop(),
    net_kernel:stop(),
    ok.

trace_tests() ->
    [fun should_trace_remote_node/0].

should_trace_remote_node() ->
    {ok, Pid} = timeit_server:new(),
    ok = timeit_server:start(Pid, [?NODE], [{string, len, '_'}]),
    5 = rpc:call(?NODE, string, len, ["hello"]),
    timer:sleep(500),
    MS = ets:fun2ms(fun({P,M,F,A,S,T}) when M == string -> {M,F,A} end),
    Matches = timeit_server:select(Pid, MS),
    ?assertEqual([{string, len, 1}], Matches).

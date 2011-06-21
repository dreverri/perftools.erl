-module(timeit).

-export([main/1]).

-record(options, {nodes=[],
                  modules=[],
                  args=[]
                 }).

main(Args) ->
    Options = parse_args(Args),
    io:format("options: ~p~n", [Options]).

parse_args(Args) ->
    parse_args(Args, #options{}).

parse_args([], Options) ->
    Args = Options#options.args,
    Options#options{args = lists:reverse(Args)};

parse_args(["-setcookie", Cookie|Args], Options) ->
    true = erlang:set_cookie(node(), list_to_atom(Cookie)),
    parse_args(Args, Options);

parse_args(["-name", Name|Args], Options) ->
    case net_kernel:start([list_to_atom(Name), longnames]) of
        {ok, _} -> ignore;
        {error, {already_started, _}} -> ignore;
        {error, Reason} -> fail_name(Reason)
    end,
    parse_args(Args, Options);

parse_args(["--node", Node|Args], Options) ->
    Nodes = Options#options.nodes,
    parse_args(Args, Options#options{nodes = [list_to_atom(Node)|Nodes]});

parse_args(["--module", MFA|Args], Options) ->
    Module = case string:tokens(MFA, ":") of
                 [M] ->
                     {list_to_atom(M), '_', '_'};
                 [M, FA] ->
                     case string:tokens(FA, "/") of
                         [F] ->
                             {list_to_atom(M), list_to_atom(F), '_'};
                         [F, A] ->
                             {list_to_atom(M), list_to_atom(F), list_to_integer(A)};
                         _ ->
                             fail_mfa(MFA)
                     end;
                 _ ->
                     fail_mfa(MFA)
             end,
    Modules = Options#options.modules,
    parse_args(Args, Options#options{modules = [Module|Modules]});

parse_args([Other|Args], Options) ->
    Others = Options#options.args,
    parse_args(Args, Options#options{args = [Other|Others]}).

fail_mfa(MFA) ->
    io:format("Invalid module: ~p~n", [MFA]),
    io:format("Expected format: module:function/arity", []),
    init:stop(1).

fail_name(Reason) ->
    io:format("Failed to start net_kernel: ~p~n", [Reason]),
    init:stop(1).

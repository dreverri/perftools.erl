-module(perftools_opts).

-export([parse/1]).

parse(Args) ->
    parse(Args, []).

parse([], Options) ->
    Fun = fun(Args) -> lists:reverse(Args) end,
    update(args, Options, [], Fun);

parse(["-setcookie", Cookie|Args], Options) ->
    true = erlang:set_cookie(node(), list_to_atom(Cookie)),
    parse(Args, Options);

parse(["-name", Name|Args], Options) ->
    case net_kernel:start([list_to_atom(Name), longnames]) of
        {ok, _} ->
            parse(Args, Options);
        {error, {already_started, _}} ->
            parse(Args, Options);
        {error, Reason} ->
            {error, {name_error, Reason}}
    end;

parse(["-node", Node|Args], Options) ->
    Fun = fun(Nodes) -> [list_to_atom(Node)|Nodes] end,
    Options1 = update(nodes, Options, [], Fun),
    parse(Args, Options1);

parse(["-function", MFA|Args], Options) ->
    case parse_mfa(MFA) of
        {error, Reason} ->
            {error, {mfa_error, Reason}};
        Function ->
            Fun = fun(Functions) -> [Function|Functions] end,
            Options1 = update(functions, Options, [], Fun),
            parse(Args, Options1)
    end;

parse([Arg|Args], Options) ->
    Fun = fun(Others) -> [Arg|Others] end,
    Options1 = update(args, Options, [], Fun),
    parse(Args, Options1).

update(Key, Proplist, Default, Fun) ->
    Value = proplists:get_value(Key, Proplist, Default),
    Value1 = Fun(Value),
    Proplist1 = proplists:delete(Key, Proplist),
    [{Key, Value1}|Proplist1].

parse_mfa(MFA) ->
    case string:tokens(MFA, ":") of
        [M] ->
            {list_to_atom(M), '_', '_'};
        [M, FA] ->
            case string:tokens(FA, "/") of
                [F] ->
                    {list_to_atom(M), list_to_atom(F), '_'};
                [F, A] ->
                    {list_to_atom(M), list_to_atom(F), list_to_integer(A)};
                _ ->
                    {error, bad_arity}
            end;
        _ ->
            {error, bad_function}
    end.

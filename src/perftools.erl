-module(perftools).

-export([main/1,
         trace/3,
         trace/4,
         stop/0,
         profile/2
        ]).

-record(profile, {chains=orddict:new(), data=[]}).

main(Args) ->
    Options = perftools_opts:parse(Args),
    run(proplists:get_value(args, Options), Options).

run(["trace", Output|_], Options) ->
    Nodes = proplists:get_value(nodes, Options, []),
    Functions = proplists:get_value(functions, Options, []),
    trace(Nodes, Functions, Output),
    io:get_chars("Press any key to stop the trace", 1);

run(["profile", Input, Output|_], _Options) ->
    profile(Input, Output);

run(_, _) ->
    io:format("Usage").

trace(Nodes, Functions, Output) ->
    trace(Nodes, Functions, Output, []).

trace(Nodes, Functions, Output, Options) ->
    case file:open(Output, [write]) of
        {ok, Handle} ->
            trace1(Nodes, Functions, Handle, Options);
        Error ->
            Error
    end.

stop() ->
    dbg:stop_clear().

profile(Input, Output) ->
    case file:open(Input, [read, binary, raw]) of
        {ok, IHandle} ->
            case file:open(Output, [write, raw]) of
                {ok, OHandle} ->
                    profile1(IHandle, OHandle);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% Tracing
%% ------------------------------------------------------------------

trace1(Nodes, Functions, Handle, _Options) ->
    case dbg:tracer(process, {fun handle_trace/2, Handle}) of
        {ok, _Pid} ->
            case dbg_nodes(Nodes) of
                ok ->
                    case dbg_tpls(Functions) of
                        ok ->
                            case dbg:p(all, [call, arity, timestamp]) of
                                {ok, _MatchDesc} ->
                                    ok;
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

handle_trace({trace_ts, Pid, return_from, MFA, _Result, Ts}, H) ->
    %% drop result
    write_msg(H, {trace_ts, Pid, return_from, MFA, <<>>, Ts});

handle_trace(Msg, H) ->
    write_msg(H, Msg).

write_msg(Handle, Msg) ->
    Bin = term_to_binary(Msg),
    Size = erlang:size(Bin),
    Data = <<Size:4/unit:8, Bin/binary>>,
    ok = file:write(Handle, Data),
    Handle.

dbg_nodes([]) ->
    ok;

dbg_nodes([N|Nodes]) when N =:= node() ->
    dbg_nodes(Nodes);

dbg_nodes([N|Nodes]) ->
    case dbg:n(N) of
        {ok, N} ->
            dbg_nodes(Nodes);
        Error ->
            Error
    end.

dbg_tpls([]) -> ok;

dbg_tpls([F|Functions]) ->
    case dbg:tpl(F, [{'_', [], [{return_trace}]}]) of
        {ok, _MatchDesc} ->
            dbg_tpls(Functions);
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% Profiling
%% ------------------------------------------------------------------

profile1(IHandle, OHandle) ->
    Profile = fold(IHandle, fun profile_fold/2, #profile{}),
    {Symbols, Iolist} = prepare(Profile#profile.data),
    write_profile(OHandle, Symbols, Iolist).

fold(IHandle, Fun, Acc) ->
    fold(IHandle, 0, Fun, Acc).

%% folds over trace messages
fold(Fh, Loc, Fun, Acc) ->
    case file:pread(Fh, Loc, 4) of
        {ok, <<Size:4/unit:8>>} ->
            case file:pread(Fh, Loc + 4, Size) of
                {ok, Bin} ->
                    Msg = binary_to_term(Bin),
                    Acc1 = Fun(Msg, Acc),
                    fold(Fh, Loc + 4 + Size, Fun, Acc1);
                _ ->
                    Acc
            end;
        _ ->
            Acc
    end.

profile_fold({trace_ts, Pid, call, {M, F, A}, Ts}, State) ->
    Chains = orddict:append(Pid, {M, F, A, Ts}, State#profile.chains),
    State#profile{chains = Chains};

profile_fold({trace_ts, Pid, return_from, {M, F, A}, _, Ts}, State) ->
    %% Look for Pid's chain
    case orddict:find(Pid, State#profile.chains) of
        {ok, Chain} ->
            %% Look for MFA in the chain
            case find_mfa({M, F, A, Ts}, lists:reverse(Chain)) of
                {ok, L, Chain0} ->
                    %% reverse chain
                    Chain1 = lists:reverse(Chain0),
                    %% record MFA latency and chain
                    Data = [{M, F, A, L, Chain1}|State#profile.data],
                    %% update chain
                    Chains = orddict:store(Pid, Chain1, State#profile.chains),
                    State#profile{data = Data, chains = Chains};
                none ->
                    %% clear Pid's chain
                    Chains = orddict:store(Pid, [], State#profile.chains),
                    State#profile{chains = Chains}
            end;
        error ->
            %% do nothing
            State
    end;

profile_fold(_Msg, State) ->
    State.

find_mfa(_, []) -> none;

find_mfa({M, F, A, End}, [{M, F, A, Start}|Chain]) ->
    Latency = timer:now_diff(End, Start),
    {ok, Latency, Chain};

find_mfa(MFA, [_|Chain]) ->
    find_mfa(MFA, Chain).

prepare(Data) ->
    Symbols0 = ets:new(symbols, []),
    prepare(Data, Symbols0, []).

prepare([], Symbols, Acc) -> {Symbols, Acc};

prepare([{M, F, A, L, C}|Data], Symbols, Acc) ->
    S = symbol(Symbols, {M,F,A}),
    Len = length(C) + 1,
    Chain = iolist_to_binary(symbols(Symbols, C)),
    Bin = <<L:64/little-integer, Len:64/little-integer,
            S:64/little-integer, Chain/binary>>,
    prepare(Data, Symbols, [Acc, Bin]).

symbol(Symbols, Key) ->
    case ets:lookup(Symbols, Key) of
        [{Key, Symbol}] ->
            Symbol;
        [] ->
            Symbol = ets:info(Symbols, size) + 1,
            ets:insert(Symbols, {Key, Symbol}),
            Symbol
    end.

symbols(Symbols, Chain) ->
    symbols(Symbols, Chain, []).

symbols(_, [], Acc) -> Acc;

symbols(Symbols, [{M,F,A,_}|Chain], Acc) ->
    S = symbol(Symbols, {M,F,A}),
    Bin = <<S:64/little-integer>>,
    symbols(Symbols, Chain, [Acc, Bin]).

write_profile(File, Symbols, Iolist) ->
    Data = ["--- symbol\n",
            symbols_data(Symbols),
            "---\n--- profile\n",
            header(),
            Iolist,
            trailer()
           ],
    file:write(File, Data).

symbols_data(Symbols) ->
    ets:foldl(fun symbol_data/2, [], Symbols).

symbol_data({MFA, Symbol}, Data) ->
    Hex = httpd_util:integer_to_hexlist(Symbol),
    [Data, ["0x", Hex, " ", mfa_to_list(MFA), "\n"]].

mfa_to_list({M,F,A}) ->
    io_lib:format("~p:~p/~p", [M, F, A]).

header() ->
    Count = 0,
    Words = 3,
    Version = 0,
    Sampling = 1000,
    Pad = 0,
    <<Count:64/little-integer, Words:64/little-integer,
      Version:64/little-integer, Sampling:64/little-integer,
      Pad:64/little-integer>>.

trailer() ->
    <<0:64/little-integer, 1:64/little-integer, 0:64/little-integer>>.

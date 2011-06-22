-module(timeit_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open/1,
         start_trace/3,
         stop_trace/1,
         fold/3,
         profile/1,
         close/1,
         stop_all/0
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {path, running=false, fh}).
-record(profile, {chains=orddict:new(), data=[]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

open(Path) ->
    gen_server:start(?MODULE, [Path], []).

start_trace(Pid, Nodes, Modules) ->
    gen_server:call(Pid, {start_trace, Nodes, Modules}).

stop_trace(Pid) ->
    gen_server:call(Pid, stop_trace).

fold(Pid, Fun, Acc) ->
    gen_server:call(Pid, {fold, Fun, Acc}).

profile(Pid) ->
    gen_server:call(Pid, profile).

close(Pid) ->
    gen_server:call(Pid, close).

stop_all() ->
    dbg:stop_clear().

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Path]) ->
    case filelib:ensure_dir(Path ++ "/foo") of
        ok ->
            {ok, #state{path=Path}};
        Error ->
            {stop, Error}
    end.

handle_call({start_trace, Nodes, Modules}, _From, State) ->
    case dbg_running() of
        false ->
            case new_trace_file(State#state.path) of
                {ok, Fh} ->
                    {ok, _} = dbg:tracer(process, {fun trace/2, {Fh, 0}}),
                    ok = dbg_nodes(Nodes),
                    {ok, _} = dbg:p(all, [call, arity, timestamp]),
                    ok = dbg_tpls(Modules),
                    {reply, ok, State#state{running=true, fh=Fh}};
                Error ->
                    {reply, Error, State}
            end;
        true ->
            {reply, {error, trace_already_running}, State}
    end;

handle_call(stop_trace, _From, State) ->
    stop_if_running(State),
    file:close(State#state.fh),
    {reply, ok, State#state{running=false}};

handle_call({fold, Fun, Acc}, _From, State) ->
    case latest_trace_file(State#state.path) of
        {ok, Fh} ->
            Acc1 = fold(Fh, 0, Fun, Acc),
            {reply, Acc1, State};
        Error ->
            {reply, Error, State}
    end;

handle_call(profile, _From, State) ->
    case latest_trace_file(State#state.path) of
        {ok, Trace} ->
            case new_profile_file(State#state.path) of
                {ok, Profile} ->
                    {Symbols, Iolist} = profile1(Trace),
                    write_profile(Profile, Symbols, Iolist),
                    {reply, ok, State};
                Error ->
                    {reply, Error, State}
            end;
        Error ->
            {reply, Error, State}
    end;

handle_call(close, _From, State) ->
    stop_if_running(State),
    {stop, normal, State#state{running=false}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

dbg_running() ->
    case whereis(dbg) of
	undefined -> false;
	_Pid -> true
    end.

new_trace_file(Path) ->
    Filename = filename:join(Path, "trace." ++ id()),
    case filelib:ensure_dir(Filename) of
        ok ->
            file:open(Filename, [read, write, binary]);
        Error ->
            Error
    end.

id() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~w~2..0w~2..0w~2..0w~2..0w~2..0w", [Y, M, D, H, Min, S]).

dbg_nodes([]) -> ok;

dbg_nodes([N|Nodes]) ->
    {ok, N} = dbg:n(N),
    dbg_nodes(Nodes).

dbg_tpls([]) -> ok;

dbg_tpls([M|Modules]) ->
    {ok, _} = dbg:tpl(M, [{'_', [], [{return_trace}]}]),
    dbg_tpls(Modules).

trace({trace_ts, Pid, return_from, MFA, _Result, Ts}, {Fh, Loc}) ->
    %% drop result
    write_msg(Fh, Loc, {trace_ts, Pid, return_from, MFA, <<>>, Ts});

trace(Msg, {Fh, Loc}) ->
    write_msg(Fh, Loc, Msg).

write_msg(Fh, Loc, Msg) ->
    Bin = term_to_binary(Msg),
    Size = erlang:size(Bin),
    Data = <<Size:4/unit:8, Bin/binary>>,
    file:pwrite(Fh, Loc, Data),
    {Fh, Loc + Size + 4}.

stop_if_running(#state{running=true}) ->
    stop_all();

stop_if_running(_) ->
    not_running.

latest_trace_file(Path) ->
    Wildcard = filename:join(Path, "trace.*"),
    case filelib:wildcard(Wildcard) of
        [] ->
            {error, no_traces};
        Traces ->
            Filename = hd(lists:reverse(lists:sort(Traces))),
            file:open(Filename, [read, binary])
    end.

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

profile1(Fh) ->
    Profile = fold(Fh, 0, fun profile_fold/2, #profile{}),
    prepare(Profile#profile.data).

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
    Symbols = ets:new(symbols, []),
    prepare(Data, Symbols, []).

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

new_profile_file(Path) ->
    Filename = filename:join(Path, "profile." ++ id()),
    case filelib:ensure_dir(Filename) of
        ok ->
            file:open(Filename, [read, write, binary]);
        Error ->
            Error
    end.

write_profile(File, Symbols, Iolist) ->
    file:write(File, "--- symbol\n"),
    write_symbols(File, Symbols),
    file:write(File, "---\n---profile\n"),
    write_header(File),
    write_data(File, Iolist),
    write_trailer(File).

write_symbols(File, Symbols) ->
    ets:foldl(fun write_symbol/2, File, Symbols).

write_symbol({MFA, Symbol}, File) ->
    Hex = httpd_util:integer_to_hexlist(Symbol),
    file:write(File, ["0x", Hex, " ", mfa_to_list(MFA), "\n"]),
    File.

mfa_to_list({M,F,A}) ->
    io_lib:format("~p:~p/~p", [M, F, A]).

write_header(File) ->
    Count = 0,
    Words = 3,
    Version = 0,
    Sampling = 1000,
    Pad = 0,
    Header = <<Count:64/little-integer, Words:64/little-integer,
               Version:64/little-integer, Sampling:64/little-integer,
               Pad:64/little-integer>>,
    file:write(File, Header).

write_data(File, Iolist) ->
    file:write(File, Iolist).

write_trailer(File) ->
    Trailer = <<0:64/little-integer, 1:64/little-integer, 0:64/little-integer>>,
    file:write(File, Trailer).

-module(timeit_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/0,
         start/3,
         select/2,
         flush/2,
         stop/0
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

-record(state, {tid, called=orddict:new()}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new() ->
    gen_server:start(?MODULE, [], []).

start(Pid, Nodes, Modules) ->
    gen_server:call(Pid, {start, Nodes, Modules}).

select(Pid, MS) ->
    gen_server:call(Pid, {select, MS}).

flush(Pid, Name) ->
    gen_server:call(Pid, {flush, Name}).

stop() ->
    dbg:stop_clear().

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    Tid = ets:new(trace, [duplicate_bag]),
    {ok, #state{tid = Tid}}.

handle_call({start, Nodes, Modules}, _From, State) ->
    case running() of
        false ->
            {ok, _} = dbg:tracer(process, {fun trace/2, self()}),
            ok = dbg_nodes(Nodes),
            {ok, _} = dbg:p(all, [call, arity, timestamp]),
            ok = dbg_tpls(Modules),
            {reply, ok, State};
        true ->
            {reply, {error, trace_already_running}, State}
    end;

handle_call({select, MS}, _From, State) ->
    {reply, ets:select(State#state.tid, MS), State};

handle_call({flush, Name}, _From, State) ->
    File = "tracers/" ++ atom_to_list(Name),
    filelib:ensure_dir(File),
    {ok, _} = dets:open_file(Name, [{file, File}, {type, duplicate_bag}]),
    ets:to_dets(State#state.tid, Name);

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({trace_ts, Pid, call, {Mod, Fun, Arity}, Ts}, S) ->
    Called = orddict:store({Pid, Mod, Fun, Arity}, Ts, S#state.called),
    {noreply, S#state{called = Called}};

handle_cast({trace_ts, Pid, return_from, {Mod, Fun, Arity}, _Result, Ts}, S) ->
    case orddict:find({Pid, Mod, Fun, Arity}, S#state.called) of
        {ok, StartTime} ->
            ElapsedUs = timer:now_diff(Ts, StartTime),
            ets:insert(S#state.tid, {Pid, Mod, Fun, Arity, StartTime, ElapsedUs});
        error ->
            ignore
    end,
    {noreply, S};

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

trace(Msg, S) ->
    gen_server:cast(S, Msg),
    S.

running() ->
    case whereis(dbg) of
	undefined -> false;
	_Pid -> true
    end.

dbg_nodes([]) -> ok;

dbg_nodes([N|Nodes]) ->
    {ok, N} = dbg:n(N),
    dbg_nodes(Nodes).

dbg_tpls([]) -> ok;

dbg_tpls([M|Modules]) ->
    {ok, _} = dbg:tpl(M, [{'_', [], [{return_trace}]}]),
    dbg_tpls(Modules).

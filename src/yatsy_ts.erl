%%%-------------------------------------------------------------------
%%% Created : 1 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : The Yatsy test server.
%%%-------------------------------------------------------------------
-module(yatsy_ts).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, start/1, start_link/1,
	 setup/1, foreach_tc/2,
	 run/0, clean/0, clean_and_run/0,
	 suite_tc_reply/1,
	 suite_doc_reply/1,
	 tc_run_reply/1,
	 fail/0, fail/1,
	 print_state/0, next_tc/0,
	 get_finished/0,
	 yaws_docroot/0, yaws_host/0, yaws_port/0, yaws_listen/0,
	 l2a/1, a2l/1
	]).

-export([test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("yatsy_ts.hrl").

-define(ilog(X,Y), error_logger:info_msg("*ilog ~p:~p: " X,
					 [?MODULE, ?LINE | Y])).

-ifdef(debug).
-define(dlog(X,Y), ?ilog(X,Y)).
-else.
-define(dlog(X,Y), true).
-endif.

-define(SERVER, ?MODULE).
-define(DEFAULT_CONF, [{cback_mod, ?MODULE}]).

-record(s, {
	  status = ?YATSY_IDLE,       % idle | running 
	  top_dir,                    % Top directory
	  remote_node = false,        % Run the test cases on this node
	  pid = false,                % Current PID of running testcase
	  timeout = ?DEFAULT_TIMEOUT, % Test case timeout
	  timer_ref = false,          % Outstanding timeout timer reference
	  error = false,              % Error reason when (status == ?YATSY_ERROR)
	  config = ?DEFAULT_CONF,     % Any external configuration {Key,Value} tuples
	  finished = [],              % list of #app{}
	  current = false,
	  queue = []                  % list of #app{}
	 }).


test() -> 
    start([{top_dir, "/home/tobbe/Kreditor/kred/lib"},
	   {remote_node, kred@sej}]).



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    start([]).

start(Config) when list(Config) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Config, []).

start_link() ->
    start_link([]).

start_link(Config) when list(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

run() ->
    gen_server:call(?SERVER, run, infinity).

clean() ->
    gen_server:call(?SERVER, clean, infinity).

clean_and_run() ->
    case clean() of
	ok   -> run();
	Else -> Else
    end.

get_finished() ->
    gen_server:call(?SERVER, get_finished, infinity).

next_tc() ->
    gen_server:call(?SERVER, next_tc, infinity).


print_state() ->
    gen_server:cast(?SERVER, print_state).


%%% Embedded yaws configuration parameters.
yaws_docroot() ->
    gen_server:call(?SERVER, yaws_docroot, infinity).

yaws_host() ->
    gen_server:call(?SERVER, yaws_host, infinity).

yaws_port() ->
    gen_server:call(?SERVER, yaws_port, infinity).

yaws_listen() ->
    gen_server:call(?SERVER, yaws_listen, infinity).



suite_tc_reply(Res) ->
    gen_server:cast(?SERVER, {suite_tc_reply, self(), Res}).

suite_doc_reply(Res) ->
    gen_server:cast(?SERVER, {suite_doc_reply, self(), Res}).

tc_run_reply(Res) ->
    gen_server:cast(?SERVER, {tc_run_reply, self(), Res}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Config) when list(Config) ->
    State = setup(Config ++ ?DEFAULT_CONF),
    remote_node_check(State).

setup(Config) ->
    TopDir = get_top_dir(Config),
    #s{top_dir     = TopDir,
       config      = Config,
       remote_node = get_remote_node(Config),
       queue       = get_apps(TopDir)
      }.

remote_node_check(#s{remote_node = false} = State) ->
    {ok, State};
%%
remote_node_check(#s{remote_node = Node} = State) ->
    case load_ourself(Node) of
	ok ->
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.

load_ourself(Node) ->
    case net_adm:ping(Node) of
	pong ->
	    ?ilog("Contacted node: ~p~n", [Node]),
	    YatsyModules = get_yatsy_modules(),
	    L = [purge_and_load(Node, Mod, Fname, Bin) ||
		    {Mod, Fname, Bin} <- YatsyModules],
	    case lists:member(false, L) of
		true ->
		    E = "failed to load Yatsy modules on node: "++a2l(Node),
		    ?ilog("~s~n", [E]),
		    {error, E};
		_ ->
		    ?ilog("Yes, loaded modules OK~n", []),
		    ok
	    end;
	_ ->
	    E = "failed to contact node: "++a2l(Node),
	    ?ilog("~s~n", [E]),
	    {error, E}
    end.

purge_and_load(Node, Mod, Fname, Bin) ->
    rpc:call(Node, code, purge, [Mod]),
    case rpc:call(Node, code, load_binary, [Mod, Fname, Bin]) of
	{module, Mod} -> true;
	_             -> false
    end.


get_yatsy_modules() ->
    Mods = [yatsy_ts, yatsy_tc],
    F = fun(M) ->
		Path = code:which(M),
		Fname = filename:basename(Path, ".beam"),
		{ok, Bin} = file:read_file(Path),
		{M, Fname, Bin}
	end,
    lists:map(F, Mods).


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(run, _From, State) when State#s.status == ?YATSY_IDLE ->
    {reply, ok, exec_tc(State)};
handle_call(run, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
%%
handle_call(clean, _From, State) when State#s.status == ?YATSY_IDLE ->
    {reply, ok, setup(State#s.config)};
handle_call(clean, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
%%
handle_call(get_finished, _From, State) ->
    {reply, {ok, State#s.finished}, State};
%%
handle_call(next_tc, _From, State) ->
    ?ilog("~n~p~n", [State]),
    {reply, next_tc(State), State};
%%
handle_call(yaws_docroot, _From, State) ->
    {reply, config(yaws_docroot, State#s.config, default_docroot()), State};
%%
handle_call(yaws_host, _From, State) ->
    {reply, config(yaws_host, State#s.config, default_host()), State};
%%
handle_call(yaws_port, _From, State) ->
    {reply, config(yaws_port, State#s.config, default_port()), State};
%%
handle_call(yaws_listen, _From, State) ->
    {reply, config(yaws_listen, State#s.config, default_listen()), State};
%%
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({tc_run_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog(" got tc_run_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    NewState = exec_tc(set_tc_rc(State#s{timer_ref = false}, Res)),
    ?ilog(" got tc_run_reply, Res=~p, ~n  NewState=~p~n", [Res,NewState]),
    %%{noreply, exec_tc(set_tc_rc(State#s{timer_ref = false}, Res))};
    {noreply, NewState};
%%
handle_cast({suite_doc_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog("got suite_doc_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    {noreply, exec_tc(set_suite_doc(State#s{timer_ref = false}, Res))};
%%
handle_cast({suite_tc_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog("got suite_tc_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    {noreply, exec_tc(set_suite_tc(State#s{timer_ref = false}, Res))};
%%
handle_cast(print_state, State) ->
    io:format("~n~p~n", [State]),
    {noreply, State};
%%
handle_cast(_Msg, State) ->
    ?ilog("handle_cast got: ~p~n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({timeout_suite_doc, Pid}, #s{pid = Pid} = State) ->
    ?dlog("timeout_suite_doc~n", []),
    {noreply, exec_tc(set_suite_doc(State, ?EMSG_FAILED_SUITE_DOC))};
%%
handle_info({timeout_suite_tc, Pid}, #s{pid = Pid} = State) ->
    ?dlog("timeout_suite_tc~n", []),
    {noreply, exec_tc(set_suite_tc(State, []))};
%%
handle_info(_Info, State) ->
    ?ilog("handle_info got: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

default_docroot() ->
    filename:join(
      lists:reverse(["docroot", "priv" | 
		     tl(lists:reverse(
			  filename:split(
			    filename:dirname(
			      code:which(yatsy)))))])).

default_host()   -> "localhost".
default_port()   -> 8888.
default_listen() -> {0,0,0,0}.
    

cancel_timer(#s{timer_ref = false}) -> false;
cancel_timer(#s{timer_ref = Tref})  -> timer:cancel(Tref).


set_suite_doc(#s{current = A} = S, {ok, Doc}) when list(Doc) ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{doc = Doc}}};
set_suite_doc(#s{current = A} = S, Else) ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{doc = Else}}}.


set_suite_tc(#s{current = A} = S, {ok, TCs}) when list(TCs)  ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{queue = TCs}}};
set_suite_tc(#s{current = A} = S, _) ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{queue = []}}}.


set_tc_rc(#s{current = A} = S, ok) ->
    Suite = A#app.current,
    TC = Suite#suite.current,
    S#s{current = A#app{current = Suite#suite{current = TC#tc{rc = ok, error = ""}}}};
set_tc_rc(#s{current = A} = S, Else) ->
    Suite = A#app.current,
    TC = Suite#suite.current,
    NewTC = TC#tc{rc = error, error = Else},
    S#s{current = A#app{current = Suite#suite{current = NewTC}}}.


%%%
%%% The Test Server Engine
%%%
exec_tc(State) ->
    ?dlog("State=~p~n", [State]),
    case next_tc(State) of
	{true, NewState} -> run_tc(NewState#s{status = ?YATSY_RUNNING});
	false            -> State#s{status = ?YATSY_IDLE}
    end.


%%
run_tc(#s{remote_node = N, current = #app{current = #suite{doc = false}}} = S) -> 
    %% Must be first time we enter this suite, so we start off by
    %% retrieving the suite doc string.
    Pid =  yatsy_tc:suite_doc_and_load(N, suite_name(S)),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_doc, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{remote_node = N, current = #app{current = #suite{queue = false}}} = S) -> 
    %% Must be second time we enter this suite, so we continue by
    %% retrieving the Test Case names of the suite.
    Pid =  yatsy_tc:suite_tc(N, suite_name(S)),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_tc, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
run_tc(S) -> 
    %% Execute a Test Case in the suite.
    run_tc(S, state2tc(S)).

run_tc(S, {true, false}) -> exec_tc(S);      % go to next Test Case
run_tc(S, {true, TC})    -> start_tc(S, TC);
run_tc(S, _)             -> S#s{status = ?YATSY_ERROR, error = ?EMSG_NO_TC}.

%%%
%%% Start running a Test Case. The Timeout is optional.
%%%
start_tc(#s{timeout = false, remote_node = N, config = Config} = S, TC) ->
    Pid = yatsy_tc:run(N, suite_name(S), TC, Config),
    S#s{pid = Pid, timer_ref = false};
start_tc(#s{timeout = Timeout, remote_node = N, config = Config} = S, TC) ->
    Pid = yatsy_tc:run(N, suite_name(S), TC, Config),
    {ok, Tref} = timer:send_after(Timeout, {timeout_tc, Pid}),
    S#s{pid = Pid, timer_ref = Tref}.


suite_name(#s{current = #app{current = #suite{name = Name}}}) -> Name.


state2tc(#s{current = #app{current = #suite{current = TC}}}) -> {true, TC};
state2tc(_)                                                  -> {true, false}.

%%%
%%%  yatsy_ts:foreach_tc(fun(X) -> io:format("~p~n", [X]) end, yatsy_ts:setup()).
%%%
foreach_tc(F, State) ->
    case next_tc(State) of
	{true, NewState} ->
	    F(state2tc(NewState)),
	    foreach_tc(F, NewState);
	false ->
	    ok
    end.

next_tc(#s{current = false, queue = []}) -> 
    false;
next_tc(#s{current = false, queue = [H|T]} = X) -> 
    next_tc(X#s{current = H, queue = T});
next_tc(#s{finished = F, current = C, queue = [H|T]} = X) -> 
    case next_tc(C) of
	{true, NewC} -> {true, X#s{current = NewC}};
	false        -> next_tc(X#s{finished = [C|F], current = H, queue = T})
    end;
next_tc(#s{finished = F, current = C, queue = []} = X) -> 
    case next_tc(C) of
	{true, NewC} -> {true, X#s{current = NewC}};
	false        -> {true, X#s{finished = [C|F], current = false}}
    end;
%%%
next_tc(#app{current = false, queue = []}) -> 
    false;
next_tc(#app{current = false, queue = [H|T]} = X) -> 
    next_tc(X#app{current = H, queue = T});
next_tc(#app{finished = F, current = C, queue = [H|T]} = X) -> 
    case next_tc(C) of
	{true, NewC} -> {true, X#app{current = NewC}};
	false        -> next_tc(X#app{finished = [C|F], current = H, queue = T})
    end;
next_tc(#app{finished = F, current = C, queue = []} = X) -> 
    case next_tc(C) of
	{true, NewC} -> {true, X#app{current = NewC}};
	false        -> {true, X#app{finished = [C|F], current = false}}
    end;
%%%
next_tc(#suite{queue = false} = X) -> % no test cases retrieved yet
    {true, X};
next_tc(#suite{current = false, queue = []}) -> 
    false;
next_tc(#suite{current = false, queue = [H|T]} = X) -> 
    {true, X#suite{current = H, queue = T}};
next_tc(#suite{finished = F, current = C, queue = [H|T]} = X) -> 
    {true, X#suite{finished =[C|F], current = H, queue = T}};
next_tc(#suite{finished = F, current = C, queue = []} = X) -> 
    {true, X#suite{finished =[C|F], current = false}}.



get_apps(TopDir) -> 
    Paths = filter_code_path(TopDir),
    F = fun(P, Acc) ->
		case get_app(P) of
		    {ok, A} -> [A|Acc];
		    _       -> Acc
		end
	end,
    lists:foldr(F, [], Paths).

filter_code_path(TopDir) ->
    Len = length(TopDir),
    F = fun(Path, Acc) -> 
		case regexp:match(Path, TopDir) of
		    {match, 1, Len} -> [Path | Acc];
		    _ -> Acc
		end
	end,
    lists:foldr(F, [], code:get_path()).


get_app(Path) ->
    case lists:reverse(string:tokens(Path, "/")) of
	["ebin", App | _] ->
	    case get_suites(Path) of
		false  -> false;
		Suites ->
		    {ok, #app{name  = App,
			      queue = Suites}}
	    end;
	_ ->
	    false
    end.

get_suites(Path) ->
    case filelib:wildcard(Path++"/*_SUITE.beam") of
	[] -> false;
	L  ->
	    [#suite{name  = filename:basename(X, ".beam"),
		    path  =  Path,
		    queue = false} || 
		X <- L]
    end.
			



fail(Reason) ->
    exit({suite_failed,Reason}).

fail() ->
    exit(suite_failed).


l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.

a2l(A) when atom(A) -> atom_to_list(A);
a2l(L) when list(L) -> L.


%%%
%%% Configuration handling
%%%

get_top_dir(Config) -> 
    case config(top_dir, Config) of
	{ok, Dir} -> Dir;
	_ ->
	    case os:getenv("YATSY_TOP_DIR") of
		false -> ".";   % fallback
		Dir   -> Dir
	    end
    end.

get_remote_node(Config) -> 
    case config(remote_node, Config) of
	{ok, Dir} -> Dir;
	_ ->
	    case os:getenv("YATSY_REMOTE_NODE") of
		false -> false;   % no rpc tp be made
		Dir   -> Dir
	    end
    end.



config(Key, L, Default) ->
    case config(Key, L) of
	{ok, Value} -> Value;
	_           -> Default
    end.

config(Key, L) when list(L) ->
    case lists:keysearch(Key, 1, L) of
	{value, {_,Value}} -> {ok, Value};
	_                  -> {error, "config: not found"}
    end.
	


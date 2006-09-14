%%%-------------------------------------------------------------------
%%% Created : 1 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : The Yatsy test server.
%%%-------------------------------------------------------------------
-module(yatsy_ts).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, start/1, start_link/1,
	 setup/1, foreach_tc/2,
	 run/0, run/1, run/2, run/3, 
	 clean/0, clean_and_run/0,
	 suite_tc_reply/1,
	 suite_doc_reply/1,
	 suite_init_reply/1, suite_fin_reply/1,
	 tc_run_reply/1,
	 tc_new_timeout/1,
	 fail/0, fail/1,
	 print_state/0, next_tc/0,
	 get_finished/0,
	 yaws_docroot/0, yaws_host/0, yaws_port/0, yaws_listen/0,
	 l2a/1, a2l/1, i2l/1, n2l/1,
	 out_dir/0, get_status/0,
	 config/3
	]).

-export([test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("yatsy_ts.hrl").


-define(SERVER, ?MODULE).
-define(DEFAULT_CONF, [{cback_mod, ?MODULE}]).
-define(BOOL_P(B), ((B == true);(B == false))).

-record(s, {
	  status = ?YATSY_IDLE,       % idle | running 
	  top_dir,                    % Top directory
	  remote_node = false,        % Run the test cases on this node
	  pid = false,                % Current PID of running testcase
	  timeout = ?DEFAULT_TIMEOUT, % Test case timeout
	  timer_ref = false,          % Outstanding timeout timer reference
	  error = false,              % Error reason when (status == ?YATSY_ERROR)
	  config = ?DEFAULT_CONF,     % Any external configuration {Key,Value} tuples
	  out_dir = ".",              % Where to put all output from Yatsy
	  quit = false,               % Quit when finished
	  interactive = false,        % Running in interactive mode
	  gen_html = false,           % Generate HTML content when finished
	  all_apps = [],
	  finished = [],              % list of #app{}
	  current = false,            % #app{}
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

run(App) ->
    gen_server:call(?SERVER, {run, App}, infinity).

run(App, Suite) ->
    gen_server:call(?SERVER, {run, App, Suite}, infinity).

run(App, Suite, TC) ->
    gen_server:call(?SERVER, {run, App, Suite, TC}, infinity).


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

get_status() ->
    gen_server:call(?SERVER, get_status, infinity).


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

out_dir() ->
    gen_server:call(?SERVER, out_dir, infinity).



suite_tc_reply(Res) ->
    gen_server:cast(?SERVER, {suite_tc_reply, self(), Res}).

suite_doc_reply(Res) ->
    gen_server:cast(?SERVER, {suite_doc_reply, self(), Res}).

suite_init_reply(Res) ->
    gen_server:cast(?SERVER, {suite_init_reply, self(), Res}).

suite_fin_reply(Res) ->
    gen_server:cast(?SERVER, {suite_fin_reply, self(), Res}).

tc_run_reply(Res) ->
    gen_server:cast(?SERVER, {tc_run_reply, self(), Res}).

tc_new_timeout(Timeout) when integer(Timeout) ->
    gen_server:cast(?SERVER, {tc_new_timeout, Timeout, self()}).



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
    ?ilog("yatsy_ts: Starting...~n", []),
    State = setup(Config ++ ?DEFAULT_CONF),
    remote_node_check(State).

setup(Config) ->
    TopDir = get_top_dir(Config),
    OutDir = get_output_dir(Config),
    Config2 = overwrite({output_dir, OutDir}, Config),
    Iact = l2bool(get_interactive(Config)),
    Config3 = overwrite({interactive, Iact}, Config2),
    Apps = get_apps(TopDir),
    #s{top_dir     = TopDir,
       out_dir     = OutDir,
       gen_html    = l2bool(get_generate_html(Config)),
       config      = Config3,
       quit        = l2bool(get_quit_when_finished(Config)),
       interactive = Iact,
       remote_node = l2a(get_remote_node(Config)),
       all_apps    = Apps,
       queue       = Apps
      }.

remote_node_check(#s{remote_node = false} = State) ->
    ?ilog("yatsy_ts: Ready...~n", []),
    {ok, State};
%%
remote_node_check(#s{remote_node = Node} = State) ->
    case load_ourself(Node) of
	ok ->
	    ?ilog("yatsy_ts: Ready...~n", []),
	    {ok, State};
	{error, Reason} ->
	    ?ilog("yatsy_ts: Failed to start, reason: ~p~n", [Reason]),
	    {stop, Reason}
    end.

load_ourself(Node) ->
    case ping(Node) of
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

ping(Node) ->
    ping(Node, 10).

ping(_, 0) -> pong;
ping(Node, N) when N>0 ->
    ?ilog("Trying to contact node: ~p~n", [Node]),
    case net_adm:ping(Node) of
	pong -> pong;
	_ ->
	    sleep(500),
	    ping(Node, N-1)
    end.

sleep(T) -> 
    receive after T -> true end.


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
handle_call(run, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
handle_call(run, _From, State) ->
    ?ilog("Yatsy starting...~n", []),
    NewState = State#s{finished = [], current = false, queue = State#s.all_apps},
    {reply, ok, exec_tc(NewState)};
%%
handle_call({run, _}, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
handle_call({run, A}, _From, State) ->
    ?ilog("Yatsy starting App=~p ...~n", [A]),
    case x_app(A, State#s.all_apps) of
	{ok, App} ->
	    NewState = State#s{finished = [], current = false, queue = [App]},
	    {reply, ok, exec_tc(NewState)};
	_ ->
	    {reply, {error, "App not found"}, State}
    end;
%%
handle_call({run, _, _}, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
handle_call({run,A,S}, _From, State) ->
    ?ilog("Yatsy starting App=~p , Suite=~p ...~n", [A,S]),
    case x_suite(A, S, State#s.all_apps) of
	{ok, Suite} ->
	    App = #app{name = A, queue = [Suite#suite{tcs_only = false}]},
	    NewState = State#s{finished = [], current = false, queue = [App]},
	    {reply, ok, exec_tc(NewState)};
	_ ->
	    {reply, {error, "Suite not found"}, State}
    end;
%%
handle_call({run, _, _, _}, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
handle_call({run,A,S,T}, _From, State) ->
    ?ilog("Yatsy starting App=~p , Suite=~p , TC=~p ...~n", [A,S,T]),
    case x_suite(A, S, State#s.all_apps) of
	{ok, Suite} ->
	    App = #app{name = A, queue = [Suite#suite{tcs_only = [l2a(T)]}]},
	    NewState = State#s{finished = [], current = false, queue = [App]},
	    {reply, ok, exec_tc(NewState)};
	_ ->
	    {reply, {error, "Suite not found"}, State}
    end;
%%
handle_call(clean, _From, State) when State#s.status == ?YATSY_RUNNING ->
    {reply, {error, "already running"}, State};
handle_call(clean, _From, State) ->
    {reply, ok, setup(State#s.config)};
%%
handle_call(get_finished, _From, State) ->
    {reply, {ok, State#s.finished}, State};
%%
handle_call(next_tc, _From, State) ->
    ?ilog("~n~p~n", [State]),
    {reply, next_tc(State), State};
%%
handle_call(get_status, _From, State) ->
    {reply, {ok, State#s.status}, State};
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
handle_call(out_dir, _From, State) ->
    {reply, {ok, State#s.out_dir}, State};
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
    {noreply, NewState};
%%
handle_cast({suite_doc_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog("got suite_doc_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    {noreply, exec_tc(set_suite_doc(State#s{timer_ref = false}, Res))};
%%
handle_cast({suite_init_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog("got suite_init_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    {noreply, exec_tc(set_suite_init(State#s{timer_ref = false}, Res))};
%%
handle_cast({suite_fin_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog("got suite_fin_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    {noreply, exec_tc(set_suite_fin(State#s{timer_ref = false}, Res))};
%%
handle_cast({suite_tc_reply, Pid, Res}, #s{pid = Pid} = State) ->
    ?dlog("got suite_tc_reply, Res=~p~n", [Res]),
    cancel_timer(State),
    {noreply, exec_tc(set_suite_tc(State#s{timer_ref = false}, Res))};
%%
handle_cast({tc_new_timeout, Timeout, Pid}, #s{pid = Pid} = State) ->
    ?ilog("got new timeout: ~p for Test Case: ~p~n", 
	  [Timeout,app_suite_tc_name(State)]),
    cancel_timer(State),
    {ok, Tref} = timer:send_after(Timeout, {timeout_tc, Pid}),
    {noreply, State#s{timer_ref = Tref}};
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
handle_info({timeout_suite_init, Pid}, #s{pid = Pid} = State) ->
    ?dlog("timeout_suite_init~n", []),
    {noreply, exec_tc(set_suite_init(State, "timedout"))};
%%
handle_info({timeout_suite_fin, Pid}, #s{pid = Pid} = State) ->
    ?dlog("timeout_suite_fin~n", []),
    {noreply, exec_tc(set_suite_fin(State, "timedout"))};
%%
handle_info({timeout_suite_tc, Pid}, #s{pid = Pid} = State) ->
    ?dlog("timeout_suite_tc~n", []),
    {noreply, exec_tc(set_suite_tc(State, []))};
%%
handle_info({timeout_tc, Pid}, #s{pid = Pid} = State) ->
    ?dlog("timeout_tc~n", []),
    {noreply, exec_tc(set_tc_rc(State, "timedout"))};
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

x_app(Name, [#app{name = Name} = A | _]) -> {ok,A};
x_app(Name, [_|T])                       -> x_app(Name, T);
x_app(_, [])                             -> {error, "not found"}.

x_suite(Aname, Sname, L) ->
    case x_app(Aname, L) of
	{ok, A} -> x_suite(Sname, A#app.queue);
	Else    -> Else
    end.

x_suite(Name, [#suite{name = Name} = S | _]) -> {ok,S};
x_suite(Name, [_|T])                         -> x_suite(Name, T);
x_suite(_, [])                               -> {error, "not found"}.



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

set_suite_init(#s{current = A} = S, {ok, Config})  ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{init = true, config = Config}}};
set_suite_init(#s{current = A} = S, Res)  ->
    Suite = A#app.current,
    ?ilog("Failed to run ~s:init_per_suite/1, reason: ~p~n", [suite_name(S), Res]),
    S#s{current = A#app{current = Suite#suite{init = true, config = []}}}.

set_suite_fin(S, ok)  ->
    S;
set_suite_fin(S, Res)  ->
    ?ilog("Failed to run ~s:fin_per_suite/1, reason: ~p~n", [suite_name(S), Res]),
    S.

set_suite_tc(#s{current = A} = S, {ok, TCs}) when list(TCs)  ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{queue = TCs}}};
set_suite_tc(#s{current = A} = S, _) ->
    Suite = A#app.current,
    S#s{current = A#app{current = Suite#suite{queue = []}}}.


set_tc_rc(#s{current = A} = S, {ok, Time}) ->
    Suite = A#app.current,
    TC = (Suite#suite.current)#tc{rc = ok, error = "", time = Time},
    S#s{current = A#app{current = Suite#suite{current = TC}}};
set_tc_rc(#s{current = A} = S, Else) ->
    Suite = A#app.current,
    TC = Suite#suite.current,
    NewTC = TC#tc{rc = error, error = Else},
    S#s{current = A#app{current = Suite#suite{current = NewTC}}}.

app_suite_tc_name(#app{name = A, current = #suite{name = S, current = #tc{name = T}}}) ->
    A++"/"++S++"/"++T;
app_suite_tc_name(_) ->
    "".

%%%
%%% The Test Server Engine
%%%
exec_tc(State) ->
    ?dlog("State=~p~n", [State]),
    case next_tc(State) of
	{true, NewState} -> run_tc(NewState#s{status = ?YATSY_RUNNING});
	false            -> finished(State#s{status = ?YATSY_FINISHED})
    end.

%%% Check if HTML should be generated
finished(#s{status = ?YATSY_FINISHED, gen_html = true} = S) ->
    ?ilog("Yatsy finished!~n", []),
    yatsy_rg:ts_is_finished(S#s.finished, S#s.out_dir, S#s.quit),
    S;
finished(S) ->
    S.


%%
run_tc(#s{remote_node = N, current = #app{current = #suite{name = M, doc = false}}} = S) -> 
    %% Must be first time we enter this suite, so we start off by
    %% retrieving the suite doc string.
    ?ilog("retrieving suite: ~s doc string...~n", [M]),
    Pid =  yatsy_tc:suite_doc_and_load(N, suite_name(S)),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_doc, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{remote_node = N, current = #app{current = #suite{name = M, init = false}}} = S) -> 
    %% Must be the second time we enter this suite, 
    %% we need to call the init_per_suite/1 function.
    ?ilog("calling ~s:init_per_suite/1 function...~n", [M]),
    Pid =  yatsy_tc:suite_init(N, suite_name(S), S#s.config),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_init, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{remote_node = N, current = #app{current = #suite{name = M, queue = false}}} = S) -> 
    %% Must be the third time we enter this suite, so we continue by
    %% retrieving the Test Case names of the suite.
    ?ilog("retrieving test cases for suite: ~s ...~n", [M]),
    Pid =  yatsy_tc:suite_tc(N, suite_name(S)),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_tc, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{remote_node = N, current = #app{current = #suite{name = M, fin = true}}} = S) -> 
    %% Must be the last time we enter this suite, so we 
    %% need to call the fin_per_suite/1
    ?ilog("calling ~s:fin_per_suite/1 function...~n", [M]),
    Sconfig = ((S#s.current)#app.current)#suite.config,
    Pid =  yatsy_tc:suite_fin(N, suite_name(S), Sconfig ++ S#s.config),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_fin, Pid}),
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
    Sconfig = ((S#s.current)#app.current)#suite.config,
    Pid = yatsy_tc:run(N, suite_name(S), TC, Sconfig ++ Config),
    S#s{pid = Pid, timer_ref = false};
start_tc(#s{timeout = Timeout, remote_node = N, config = Config} = S, TC) ->
    Sconfig = ((S#s.current)#app.current)#suite.config,
    Pid = yatsy_tc:run(N, suite_name(S), TC, Sconfig ++ Config),
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
next_tc(#suite{current = false, queue = [], fin = false} = X) -> % setup M:fin_per_suite/1
    {true, X#suite{fin = true}};
next_tc(#suite{current = false, queue = []}) -> 
    false;
next_tc(#suite{current = false, queue = [H|T]} = X) -> 
    case tc_ok(H, X#suite.tcs_only) of
	true -> {true, X#suite{current = H, queue = T}};
	_    -> next_tc(X#suite{current = H, queue = T})
    end;
next_tc(#suite{finished = F, current = C, queue = [H|T]} = X) -> 
    case tc_ok(H, X#suite.tcs_only) of
	true -> {true, X#suite{finished =[C|F], current = H, queue = T}};
	_    -> next_tc(X#suite{finished =[C|F], current = H, queue = T})
    end;
next_tc(#suite{finished = F, current = C, queue = []} = X) -> 
    {true, X#suite{finished =[C|F], current = false}}.

%%% Check if it is ok to run this test case
tc_ok(_, false)                   -> true;
tc_ok(#tc{name = Name}, [Name|_]) -> true;
tc_ok(TC, [_|T])                  -> tc_ok(TC, T);
tc_ok(_, [])                      -> false.
    

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

i2l(I) when integer(I) -> integer_to_list(I);
i2l(L) when list(L)    -> L.

n2l(I) when integer(I) -> integer_to_list(I);
n2l(F) when float(F)   -> float_to_list(F);
n2l(L) when list(L)    -> L.

l2bool("true") -> true;
l2bool(true)   -> true;
l2bool(_)      -> false.


%%%
%%% Configuration handling
%%%

get_top_dir(Config) -> 
    get_config_param("YATSY_TOP_DIR", top_dir, Config).

get_remote_node(Config) -> 
    get_config_param("YATSY_REMOTE_NODE", remote_node, Config).

get_output_dir(Config) -> 
    get_config_param("YATSY_OUTPUT_DIR", output_dir, Config).

get_generate_html(Config) -> 
    get_config_param("YATSY_GENERATE_HTML", generate_html, Config).

get_quit_when_finished(Config) -> 
    get_config_param("YATSY_QUIT_WHEN_FINISHED", quit_when_finished, Config).

get_interactive(Config) -> 
    get_config_param("YATSY_INTERACTIVE", interactive, Config).


get_config_param(EnvVar, Key, Config) -> 
    case config(Key, Config) of
	{ok, Value} -> Value;
	_ ->
	    case os:getenv(EnvVar) of
		false -> false;   
		Value -> Value
	    end
    end.

overwrite({K,V},[{K,_}|T]) -> [{K,V}|T];
overwrite(E, [H|T])        -> [H | overwrite(E, T)];
overwrite(E, [])           -> [E].

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
	



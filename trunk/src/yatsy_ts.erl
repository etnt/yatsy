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
	 output_dir/0, cc_output_dir/0, get_status/0, 
	 run_in_remote_node/0, target_node/0,
	 config/3, err/1
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
	  status = ?YATSY_IDLE,       % idle | running | finished
	  top_dir,                    % Top directory
	  target_node = false,        % Name of target node
	  run_in_remote_node = false, % false: run in this node, true: run in target node
	  pid = false,                % Current PID of running testcase
	  timeout = ?DEFAULT_TIMEOUT, % Test case timeout
	  timer_ref = false,          % Outstanding timeout timer reference
	  error = false,              % Error reason when (status == ?YATSY_ERROR)
	  config = ?DEFAULT_CONF,     % Any external configuration {Key,Value} tuples
	  email = false,              % Email address where to send mail in case of errors
	  output_dir = ".",           % Where to put all output from Yatsy
	  cc_output_dir = ".",        % Where to put cruise control output from Yatsy
 	  quit = false,               % Quit when finished
	  interactive = false,        % Running in interactive mode
	  gen_html = false,           % Generate HTML content when finished
	  gen_cc = true,              % Generate cc-style XML file when finished
	  all_apps = [],
	  finished = [],              % list of #app{}
	  current = false,            % #app{}
	  queue = [],                 % list of #app{}
	  target_dir                  % top dir of target code
	 }).


test() -> 
    start([{top_dir, "/home/tobbe/Kreditor/kred/lib"},
	   {target_node, kred@sej},
	   {run_in_remote_node, true}]).



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

output_dir() ->
    gen_server:call(?SERVER, output_dir, infinity).

cc_output_dir() ->
    gen_server:call(?SERVER, cc_output_dir, infinity).

run_in_remote_node() ->
    gen_server:call(?SERVER, run_in_remote_node, infinity).

target_node() ->
    gen_server:call(?SERVER, target_node, infinity).



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
    ?ilog("yatsy_ts: Starting...~p ~n", [Config]),
    State = setup(Config ++ ?DEFAULT_CONF),
    target_node_check(State).

setup(Config) ->
    TopDir = get_top_dir(Config),
    TargetDir = get_target_dir(Config),
    %%
    OutDir = get_output_dir(Config),
    Config2 = overwrite({output_dir, OutDir}, Config),
    %%
    Iact = l2bool(get_interactive(Config)),
    Config3 = overwrite({interactive, Iact}, Config2),
    %%
    YawsHost = get_yaws_host(Config, default_host()),
    Config4 = overwrite({yaws_host, YawsHost}, Config3),
    %%
    YawsPort = get_yaws_port(Config, default_port()),
    Config5 = overwrite({yaws_port, YawsPort}, Config4),
    %%
    YawsListen = mk_listen(get_yaws_listen(Config, default_listen())),
    Config6 = overwrite({yaws_listen, YawsListen}, Config5),
    %%
    CCOutDir = get_cc_output_dir(Config),
    Config7 = overwrite({cc_output_dir, CCOutDir}, Config6),
    %%
    Email = get_email(Config, false),
    %%
    TargetNode = l2a(get_target_node(Config)),
    Apps = get_apps(TargetNode, TargetDir),
    ?ilog("****** (TargetNode=~p, TargetDir=~p) Found ~p applications~n", 
	  [TargetNode, TargetDir, length(Apps)]),
    Config8 = overwrite({yatsy_target_node, TargetNode}, Config7),
    #s{top_dir     = TopDir,
       target_dir  = TargetDir,
       output_dir  = OutDir,
       cc_output_dir  = CCOutDir,
       gen_html    = l2bool(get_generate_html(Config)),
       gen_cc      = l2bool(get_generate_cc(Config)),
       config      = Config8,
       email       = Email,
       quit        = l2bool(get_quit_when_finished(Config)),
       interactive = Iact,
       target_node = TargetNode,
       run_in_remote_node = bool_check("run_in_remote_node", l2a(get_run_in_remote_node(Config))),
       all_apps    = Apps,
       queue       = Apps
      }.

bool_check(_What, Bool) when Bool==true;Bool==false -> Bool;
bool_check(What, _Bool) when list(What) ->
    ?ilog("*** WARNING ***: Not bolean value(~s)~n", [What]),
    false.


mk_listen(T) when tuple(T) -> T;
mk_listen(L) when list(L) ->
    [A,B,C,D] = string:tokens(L, "{}, "),
    {list_to_integer(A),list_to_integer(B),list_to_integer(C),list_to_integer(D)}.

%%
%% No target node to use!
%%
target_node_check(#s{target_node = false} = State) ->
    ?ilog("yatsy_ts: Ready...~n", []),
    {ok, State};
%%
%% Don't run anything in the target node!
%%
target_node_check(#s{run_in_remote_node = false} = State) ->
    ?ilog("yatsy_ts: Ready...~n", []),
    {ok, State};
%%
%% We are supposed to run things in the target node!
%%
target_node_check(#s{target_node = Node} = State) ->
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

ping(_, 0) -> pang;
ping(Node, N) when N>0 ->
    ?ilog("Trying to contact node: ~p~n", [Node]),
    case net_adm:ping(Node) of
	pong ->
	    wait_started(Node);
	_ ->
	    sleep(500),
	    ping(Node, N-1)
    end.

wait_started(Node) ->
    case rpc:call(Node, init, get_status, []) of
	{started, started} ->
	    pong;
	{starting, _} ->
	    sleep(500),
	    wait_started(Node);
	_ ->
	    pang
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
    {reply, false, exec_tc(NewState)};
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
	    ?ilog("+++ NOT FOUND SUITE=~p , AllApps=~p~n", [S, State#s.all_apps]),
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
handle_call(output_dir, _From, State) ->
    {reply, {ok, State#s.output_dir}, State};
%%
handle_call(cc_output_dir, _From, State) ->
    {reply, {ok, State#s.cc_output_dir}, State};
%%
handle_call(target_node, _From, State) ->
    {reply, {ok, State#s.target_node}, State};
%%
handle_call(run_in_remote_node, _From, State) ->
    {reply, {ok, State#s.run_in_remote_node}, State};
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
    ?dlog("got new timeout: ~p for Test Case: ~p~n", 
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
    ?ilog("~p returned ok", [TC#tc.name]),
    S#s{current = A#app{current = Suite#suite{current = TC}}};
set_tc_rc(#s{current = A} = S, Else) ->
    Suite = A#app.current,
    TC = Suite#suite.current,
    NewTC = TC#tc{rc = error, error = Else},
    ?ilog("~p failed", [TC#tc.name]),
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
finished(#s{} = S) ->
    #s{status = ?YATSY_FINISHED, 
       gen_html = GenHTML, 
       gen_cc = GenCC,
       output_dir = OutDir,
       cc_output_dir = CCOutDir} = S,
    case GenHTML of 
	true  -> yatsy_rg:gen_html(S#s.finished, OutDir);
	false -> ok
    end,
    case GenCC of 
	true  -> yatsy_rg:gen_cc(S#s.finished, CCOutDir);
	false -> ok
    end,
    yatsy_rg:ts_is_finished(S#s.quit),
    maybe_sendmail(S),
    ?ilog("Yatsy finished!~n", []),
    S.

maybe_sendmail(S) ->
    case any_errors(S) of
	true when list(S#s.email) -> 
	    yatsy_sendmail:send(S#s.email,
				"yatsy",
				"Error in Yatsy output",
				"Error in Yatsy output:\n\n"
				"  Date & Time : "++nice_date_time()++"\n"
				"  Target Node : "++to_list(S#s.target_node)++"\n"
				"  Output Dir  : "++S#s.output_dir++"\n"
				"  Summary     : \n\n"++ err_msg(S) ++"\n"
			       );
	_ when list(S#s.email) ->
	    yatsy_sendmail:send(S#s.email,
				"yatsy",
				"Successful Yatsy output",
				"Yatsy completed successfully:\n\n"
				"  Date & Time : "++nice_date_time()++"\n"
				"  Target Node : "++to_list(S#s.target_node)++"\n"
				"  Output Dir  : "++S#s.output_dir++"\n"
			       );
	_ ->
	    true
    end.



any_errors(S) ->
    lists:member(true, [err(X) || X <- S#s.finished]).

err(#app{finished=Suites}) ->
    lists:member(true, [err(X) || X <- Suites]);
err(#suite{finished=TCs}) ->
    lists:member(true, [err(X) || X <- TCs]);
err(#tc{rc=ok}) ->
    false;
err(#tc{rc=_Else}) ->
    true.

err_msg(#s{finished=Apps}) -> 
    lists:foldl(fun(#app{name = Name} = A, Output) -> 
			case err(A) of
			    true  -> "=" ++ Name ++ "=\n" ++ 
					 err_msg(A) ++ "\n" ++ Output;
			    false -> Output
			end
		end,
		"", Apps);

err_msg(#app{finished=Suites}) -> 
    lists:foldl(fun(#suite{name = Name} = Suite, Output) -> 
			case err(Suite) of
			    true  -> "==" ++ Name ++ "==\n" ++ 
					 err_msg(Suite)++ "\n" ++ Output;
			    false -> Output
			end
		end,
		"", Suites);

err_msg(#suite{finished=TCs}) -> 
    err_msg(TCs);

err_msg(TCs) -> err_msg_aux(TCs).

err_msg_aux(TCs) -> err_msg_aux(TCs, []).

err_msg_aux([], Errors) -> lists:flatten(lists:reverse(Errors));
err_msg_aux([#tc{rc=ok}|TCs], Errors) -> err_msg_aux(TCs, Errors);
err_msg_aux([#tc{error=Error, name=Name}|TCs], Errors) -> 
    NewError = "===" ++ atom_to_list(Name) ++ "=== \n" ++ 
	io_lib:format("~p~n", [Error]),
    err_msg_aux(TCs, [NewError|Errors]).

%%
run_tc(#s{current = #app{current = #suite{name = M, doc = false}}} = S) -> 
    %% Must be first time we enter this suite, so we start off by
    %% retrieving the suite doc string.
    ?ilog("retrieving suite: ~s doc string...~n", [M]),
    Pid =  yatsy_tc:suite_doc_and_load(target_node(S), suite_name(S)),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_doc, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{current = #app{current = #suite{name = M, init = false}}} = S) -> 
    %% Must be the second time we enter this suite, 
    %% we need to call the init_per_suite/1 function.
    ?ilog("calling ~s:init_per_suite/1 function...~n", [M]),
    Pid =  yatsy_tc:suite_init(target_node(S), suite_name(S), S#s.config),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_init, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{current = #app{current = #suite{name = M, queue = false}}} = S) -> 
    %% Must be the third time we enter this suite, so we continue by
    %% retrieving the Test Case names of the suite.
    ?ilog("retrieving test cases for suite: ~s ...~n", [M]),
    Pid =  yatsy_tc:suite_tc(target_node(S), suite_name(S)),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_tc, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
%%
run_tc(#s{current = #app{current = #suite{name = M, fin = true}}} = S) -> 
    %% Must be the last time we enter this suite, so we 
    %% need to call the fin_per_suite/1
    ?ilog("calling ~s:fin_per_suite/1 function...~n", [M]),
    Sconfig = ((S#s.current)#app.current)#suite.config,
    Pid =  yatsy_tc:suite_fin(target_node(S), suite_name(S), Sconfig ++ S#s.config),
    {ok, Tref} = timer:send_after(?DEFAULT_TIMEOUT, {timeout_suite_fin, Pid}),
    S#s{pid = Pid, timer_ref = Tref};
run_tc(S) -> 
    %% Execute a Test Case in the suite.
    ?dlog("calling TestCase: ~p~n", [state2tc_name(S)]),
    run_tc(S, state2tc(S)).


target_node(#s{run_in_remote_node = true, target_node = Node}) -> Node;
target_node(_)                                                 -> false.


run_tc(S, {true, false}) -> exec_tc(S);      % go to next Test Case
run_tc(S, {true, TC})    -> start_tc(S, TC);
run_tc(S, _)             -> S#s{status = ?YATSY_ERROR, error = ?EMSG_NO_TC}.

%%%
%%% Start running a Test Case. The Timeout is optional.
%%%
start_tc(#s{timeout = false, config = Config} = S, TC) ->
    Sconfig = ((S#s.current)#app.current)#suite.config,
    Pid = yatsy_tc:run(target_node(S), suite_name(S), TC, Sconfig ++ Config),
    S#s{pid = Pid, timer_ref = false};
start_tc(#s{timeout = Timeout, config = Config} = S, TC) ->
    Sconfig = ((S#s.current)#app.current)#suite.config,
    Pid = yatsy_tc:run(target_node(S), suite_name(S), TC, Sconfig ++ Config),
    {ok, Tref} = timer:send_after(Timeout, {timeout_tc, Pid}),
    S#s{pid = Pid, timer_ref = Tref}.


suite_name(#s{current = #app{current = #suite{name = Name}}}) -> Name.


state2tc(#s{current = #app{current = #suite{current = TC}}}) -> {true, TC};
state2tc(_)                                                  -> {true, false}.

state2tc_name(#s{current = #app{current = #suite{current = TC}}}) ->
    case TC of
	#tc{name = Name} -> Name;
	A -> {debug, A}
    end;
state2tc_name(Term) -> {debug, Term}.

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
	false        -> RevFinished = lists:reverse([C|F]),
			{true, X#s{finished = RevFinished, current = false}}
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
	false        -> RevFinished = lists:reverse([C|F]),
			{true, X#app{finished = RevFinished, current = false}}
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
    {true, X#suite{finished = lists:reverse([C|F]), current = false}}.

%%% Check if it is ok to run this test case
tc_ok(_, false)                   -> true;
tc_ok(#tc{name = Name}, [Name|_]) -> true;
tc_ok(TC, [_|T])                  -> tc_ok(TC, T);
tc_ok(_, [])                      -> false.
    

get_apps(Node, Dir) when Node == node() -> 
    get_apps(Dir);
get_apps(Node, Dir) -> 
    Ps = rpc:call(Node, code, get_path, []),
    lists:foreach(fun(P) -> code:add_path(P) end,
		  Ps -- code:get_path()),
    get_apps(Dir).

get_apps(Dir) -> 
    Paths = filter_code_path(Dir),
    F = fun(P, Acc) ->
		case get_app(P) of
		    {ok, A} -> [A|Acc];
		    _       -> Acc
		end
	end,
    lists:foldr(F, [], Paths).

filter_code_path(Dir) ->
    Len = length(Dir),
    F = fun(Path, Acc) -> 
		case regexp:match(Path, Dir) of
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

get_target_dir(Config) -> 
    get_config_param("YATSY_TARGET_DIR", target_dir, Config).

get_target_node(Config) -> 
    get_config_param("YATSY_TARGET_NODE", target_node, Config).

get_run_in_remote_node(Config) -> 
    get_config_param("YATSY_RUN_IN_REMOTE_NODE", run_in_remote_node, Config).

get_output_dir(Config) -> 
    get_config_param("YATSY_OUTPUT_DIR", output_dir, Config).

get_cc_output_dir(Config) -> 
    get_config_param("YATSY_CC_OUTPUT_DIR", output_dir, Config).

get_generate_html(Config) -> 
    get_config_param("YATSY_GENERATE_HTML", generate_html, Config).

get_generate_cc(Config) -> 
    get_config_param("YATSY_GENERATE_CC", generate_cc, Config).

get_quit_when_finished(Config) -> 
    get_config_param("YATSY_QUIT_WHEN_FINISHED", quit_when_finished, Config).

get_interactive(Config) -> 
    get_config_param("YATSY_INTERACTIVE", interactive, Config).

get_yaws_host(Config, Default) -> 
    get_config_param("YATSY_YAWS_HOST", yaws_host, Config, Default).

get_yaws_port(Config, Default) -> 
    get_config_param("YATSY_YAWS_PORT", yaws_port, Config, Default).

get_yaws_listen(Config, Default) -> 
    get_config_param("YATSY_YAWS_LISTEN", yaws_listen, Config, Default).

get_email(Config, Default) -> 
    get_config_param("YATSY_EMAIL", email, Config, Default).


get_config_param(EnvVar, Key, Config) -> 
    case config(Key, Config) of
	{ok, Value} -> Value;
	_ ->
	    case os:getenv(EnvVar) of
		false -> false;   
		Value -> Value
	    end
    end.

get_config_param(EnvVar, Key, Config, Default) -> 
    case config(Key, Config) of
	{ok, Value} -> Value;
	_ ->
	    case os:getenv(EnvVar) of
		false -> Default;   
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
	


    
-define(il(I), integer_to_list(I)).

nice_date_time() ->
    nice_date()++" , "++nice_time().

nice_date() ->
    {Y,M,D} = date(),
    ?il(Y)++"-"++?il(M)++"-"++?il(D).

nice_time() ->
   {H,N,S} = time(),
    ?il(H)++":"++?il(N)++":"++?il(S).

-undef(il).

to_list(X) when atom(X)    -> atom_to_list(X);
to_list(X) when integer(X) -> integer_to_list(X);
to_list(X) when float(X)   -> float_to_list(X);
to_list(X) when list(X)    -> X.

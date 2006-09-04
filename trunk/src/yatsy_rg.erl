%%%-------------------------------------------------------------------
%%% Created :  4 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : Yatsy Report Generator.
%%%-------------------------------------------------------------------
-module(yatsy_rg).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0,
	 top/1, app/2, suite/3, tc/4
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(yatsy_ts, [a2l/1, l2a/1]).

-include("yatsy_ts.hrl").

-define(SERVER, ?MODULE).

-record(s, {}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

top(Url) ->
    gen_server:call(?SERVER, {top, Url}, infinity).

app(Url, App) ->
    gen_server:call(?SERVER, {app, Url, App}, infinity).

suite(Url, App, Suite) ->
    gen_server:call(?SERVER, {suite, Url, App, Suite}, infinity).

tc(Url, App, Suite, Tc) ->
    gen_server:call(?SERVER, {tc, Url, App, Suite, l2a(Tc)}, infinity).

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
init([]) ->
    {ok, #s{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({top, Url}, _From, State) ->
    {ok, Finished} = yatsy_ts:get_finished(),
    {reply, do_top(Url, Finished), State};
%%
handle_call({app, Url, App}, _From, State) ->
    {ok, Finished} = yatsy_ts:get_finished(),
    {reply, do_app(Url, App, Finished), State};
%%
handle_call({suite, Url, App, Suite}, _From, State) ->
    {ok, Finished} = yatsy_ts:get_finished(),
    {reply, do_suite(Url, App, Suite, Finished), State};
%%
handle_call({tc, Url, App, Suite, Tc}, _From, State) ->
    {ok, Finished} = yatsy_ts:get_finished(),
    {reply, do_tc(Url, App, Suite, Tc, Finished), State};
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
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

do_top(Url, Apps) ->
    {'div', [{id, "yatsy_top"}],
     yatsy_ehtml:table(["Application"], 
		       [[mk_link(Url, "app", Aname)] || 
			   #app{name = Aname} <- Apps])}.


do_app(Url, App, Apps) ->
    case get_app(App, Apps) of
	{ok, A} ->
	    TDs = [[mk_link(Url, "suite", Sname), Doc] || 
		      #suite{name = Sname, doc = Doc} <- A#app.finished],
	    {'div', [{id, "yatsy_app"}],
	     yatsy_ehtml:table(["Suite", "Description"], TDs)};
	_ ->
	    {'div', [{id, "yatsy_error"}],
	     "no application found"}
    end.


do_suite(Url, App, Suite, Apps) ->
    case get_app(App, Apps) of
	{ok, A} ->
	    case get_suite(Suite, A#app.finished) of
		{ok, S} ->
		    TDs = [[mk_link(Url, "tc", a2l(Tname)), a2l(RC), Doc] || 
			      #tc{name = Tname, doc = Doc, rc = RC} <- S#suite.finished],
		    {'div', [{id, "yatsy_suite"}],
		     yatsy_ehtml:table(["Test case","Result","Description"],TDs)};
		_ ->
		    {'div', [{id, "yatsy_error"}],
		     "no suite found"}
	    end;
	_ ->
	    {'div', [{id, "yatsy_error"}],
	     "no application found"}
    end.

do_tc(_Url, App, Suite, Tc, Apps) ->
    case get_app(App, Apps) of
	{ok, A} ->
	    case get_suite(Suite, A#app.finished) of
		{ok, S} ->
		    Res = case get_tc(Tc, S#suite.finished) of
			      {ok, #tc{rc = ok} = T} ->
				  ["(Description: ", {i, [], T#tc.doc}, ")",
				   {br, []},
				   "ok"];
			      {ok, #tc{rc = error} = T} ->
				  ["(Description: ", {i, [], T#tc.doc}, ")",
				   {br, []},
				   lists:flatten(io_lib:format("~p", [T#tc.error]))];
			      _ -> 
				  "no test case found"
			  end,
		    {'div', [{id, "yatsy_tc"}], [{p, [], Res}]};
		_ ->
		    {'div', [{id, "yatsy_error"}],
		     "no suite found"}
	    end;
	_ ->
	    {'div', [{id, "yatsy_error"}],
	     "no application found"}
    end.


get_app(App, [#app{name = App} = H|_]) -> {ok, H};
get_app(App, [_|T])                    -> get_app(App, T);
get_app(_, [])                         -> {error, "not found"}.

get_suite(Suite, [#suite{name = Suite} = H|_]) -> {ok, H};
get_suite(Suite, [_|T])                        -> get_suite(Suite, T);
get_suite(_, [])                               -> {error, "not found"}.

get_tc(Tc, [#tc{name = Tc} = H|_]) -> {ok, H};
get_tc(Tc, [_|T])                  -> get_tc(Tc, T);
get_tc(_, [])                      -> {error, "not found"}.


mk_link(Url, Key, Aname) ->
    case qargs_p(Url) of
	true -> {a, [{href, Url++"&"++Key++"="++Aname}], Aname};
	_    -> {a, [{href, Url++"?"++Key++"="++Aname}], Aname}
    end.

%%% Do the Url contain any '?' , i.e query args ?
qargs_p(Url) ->
    case string:chr(Url, $?) of
	0 -> false;
	_ -> true
    end.

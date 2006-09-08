%%%-------------------------------------------------------------------
%%% Created :  4 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : Yatsy Report Generator.
%%%-------------------------------------------------------------------
-module(yatsy_rg).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0,
	 top/1, app/2, suite/3, tc/4,
	 ts_is_finished/3,
	 html/1, style/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(yatsy_ts, [a2l/1, l2a/1]).
-import(yaws_api, [ehtml_expand/1]).

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

ts_is_finished(Finished, OutDir, Quit) ->
    gen_server:cast(?SERVER, {ts_is_finished, Finished, OutDir, Quit}).


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
handle_cast({ts_is_finished, Finished, OutDir, Quit}, State) ->
    produce_html_output(Finished, OutDir),
    ?ilog("Wrote HTML output!~n", []),
    if (Quit == true) -> 
	    ?ilog("Stopping yatsy...!~n", []),
	    init:stop();
       true -> false
    end,
    {noreply, State};
%%
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

produce_html_output(Finished, OutDir) ->
    HtmlDir = filename:join([OutDir, "html"]),
    os:cmd("mkdir "++HtmlDir),
    mk_top_page(Finished, HtmlDir),
    mk_app_pages(Finished, HtmlDir),
    mk_suite_pages(Finished, HtmlDir),
    mk_tc_pages(Finished, HtmlDir).

mk_top_page(Finished, HtmlDir) -> 
    Page = ehtml_expand(html([{h1, [], "The Application Page"},
			      do_top(fun(Name) -> html_app_fname(Name) end,
				     Finished)])),     
    Fname = filename:join([HtmlDir, "index.html"]),
    file:write_file(Fname, list_to_binary(lists:flatten(Page))).


mk_app_pages(Finished, HtmlDir) ->
    Ps = [{A#app.name,
	   ehtml_expand(html([{h1, [], "Test Suites in application: "++A#app.name},
			      do_app(fun html_suite_fname/1,
				     A#app.name,
				     Finished)]))} ||
	     A <- Finished],
    [write_app_page(HtmlDir, A) || A <- Ps].

write_app_page(HtmlDir, {Aname, Page}) ->
    Fname = filename:join([HtmlDir, html_app_fname(Aname)]),
    file:write_file(Fname, list_to_binary(lists:flatten(Page))).


mk_suite_pages(Finished, HtmlDir) ->
    lists:foreach(fun(A) -> mk_suite_page(Finished, HtmlDir, A) end, Finished).

mk_suite_page(Finished, HtmlDir, A) when record(A, app) ->
    Ps = [{S#suite.name,
	   ehtml_expand(html([{h1, [], "Test Cases in test suite: "++A#app.name++"/"++S#suite.name},
			      do_suite(fun(Tname) -> html_tc_fname(S#suite.name, Tname) end,
				       A#app.name,
				       S#suite.name,
				       Finished)]))} ||
	     S <- A#app.finished],
    [write_suite_page(HtmlDir, P) || P <- Ps].

write_suite_page(HtmlDir, {Name, Page}) ->
    Fname = filename:join([HtmlDir, html_suite_fname(Name)]),
    file:write_file(Fname, list_to_binary(lists:flatten(Page))).



mk_tc_pages(Finished, HtmlDir) ->
    lists:foreach(fun(A) -> 
			  lists:foreach(fun(S) ->
						mk_tc_page(Finished, HtmlDir, 
							   A#app.name, S) 
					end, A#app.finished)
		  end, Finished).

mk_tc_page(Finished, HtmlDir, Aname, S) when record(S, suite) ->
    Ps = [{a2l(T#tc.name),
	   S#suite.name,
	   ehtml_expand(html([{h1, [], "Test Cases: "++Aname++"/"++S#suite.name++"/"++a2l(T#tc.name)},
			      do_tc("",
				    Aname,
				    S#suite.name,
				    T#tc.name,
				    Finished)]))} ||
	     T <- S#suite.finished],
    [write_tc_page(HtmlDir, P) || P <- Ps].

write_tc_page(HtmlDir, {Name, Sname, Page}) ->
    Fname = filename:join([HtmlDir, html_tc_fname(Sname, Name)]),
    file:write_file(Fname, list_to_binary(lists:flatten(Page))).



html_app_fname(Aname) -> 
    "yatsy_app_"++Aname++".html".

html_suite_fname(Sname) -> 
    "yatsy_suite_"++Sname++".html".

html_tc_fname(Sname, Tname) -> 
    "yatsy_tc_"++Sname++"_"++Tname++".html".


html(Body) ->
    [{html, [],
      [{head, [],
	[style()]},
       {body, [], 
	{'div', [{id, "yatsy_output"}], Body}}]}].

style() ->
    {style, [],
     ["#yatsy_output {margin-left: 20px; margin-top: 40px}\n"
      "#yatsy_output table {border-collapse: collapse}\n"
      "#yatsy_output td {border: 1px solid black; padding: 3px 6px 3px 6px}\n"
      "#yatsy_output th {border: 1px solid black; text-align: left; padding: 3px 6px 3px 6px}\n"
      "#yatsy_output h1 {font-size: x-large; padding-bottom: 10px}\n"
      ]}.





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


%%%
%%% Either we generate one html page per app/suite/tc,
%%% or we have one yatsy.yaws page + query arguments.
%%%
mk_link(F, _Key, Name) when function(F) ->
    {a, [{href, F(Name)}], Name};
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

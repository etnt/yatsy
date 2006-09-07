%%%-------------------------------------------------------------------
%%% Created :  4 Sep 2006 by Torbjorn Tornkvist <tobbe@sej.hq.kred>
%%% Descr.  : (yatsy) Yet Another Test Server - Yaws compatible 
%%%
%%% @doc YATSY: Yet Another Test Server - Yaws compatible 
%%%     
%%% <p>Yatsy is a simple test framework for testing your Erlang code.</p>
%%%
%%% <p>Basically, what you do is:
%%%   <ol>
%%%     <li>Write Erlang code that tests the functionality 
%%%         of your system.</li>
%%%
%%%     <li>Put the code in an xxx_SUITE.erl file.</li>
%%%
%%%     <li>Make sure the xxx_SUITE.erl file is compiled.</li>
%%%
%%%     <li>Checkout and compile Yatsy.</li>
%%%
%%%     <li>Start your system and make sure that the code path 
%%%         includes the .../yatsy/ebin directory.</li>
%%%
%%%     <li>Run: yatsy:quick().</li>
%%%
%%%     <li>Point a Web Browser to http://localhost:8888/yatsy.yaws
%%%         and study the result from the test cases.</li>
%%%
%%%   </ol>
%%% </p>
%%%
%%%
%%% @author  Torbjörn Törnkvist <tobbe@tornkvist.org>
%%% @end
%%%-------------------------------------------------------------------
-module(yatsy).

-export([start/0, start_link/0, start/1, start_link/1,
	 run/0, clean/0, clean_and_run/0,
	 top/1, app/2, suite/3, tc/4,
	 start_yaws/0,
	 quick/0, quick/1
	 ]).

-export([test/0]).

test() ->
    yatsy_ts:test(),
    yatsy_rg:start(),
    start_yaws(),
    yatsy_ts:run().


%%%
%%% Start Yaws in embedded mode.
%%%
start_yaws() ->
    DocRoot = yatsy_ts:yaws_docroot(),
    {ok, OutDir}  = yatsy_ts:out_dir(),
    Host    = yatsy_ts:yaws_host(),
    Port    = yatsy_ts:yaws_port(),
    Listen  = yatsy_ts:yaws_listen(),
    SL = [{host, Host}, {port, Port}, {listen, Listen}],
    GL = [{logdir, OutDir}],
    io:format(" ++++++ yaws:start_embedded(~p,~p,~p).~n", [DocRoot, SL, GL]),
    yaws:start_embedded(DocRoot, SL, GL).

    
%%%
%%% Yatsy Test Server interface
%%%
quick() ->
    quick([]).

quick(Config) ->
    yatsy_ts:start(Config),
    wait_for_yatsy_ts(),
    yatsy_rg:start(),
    sleep(100),
    start_yaws(),
    sleep(100),
    yatsy_ts:run().

%%% In case of a remote node; Yatsy talks to that node
%%% which may take some time to finished. Hence, we 
%%% are polling for it to become started.
wait_for_yatsy_ts() ->
    case catch yatsy_ts:get_status() of
	{ok, _} ->
	    true;
	_ ->
	    sleep(500),
	    wait_for_yatsy_ts()
    end.

sleep(T) -> 
    receive after T -> true end.

start() ->
    start([]).

start(Config) ->
    yatsy_ts:start(Config),
    yatsy_rg:start().

start_link() ->
    start_link([]).

start_link(Config) ->
    yatsy_ts:start_link(Config),
    yatsy_rg:start_link().

run() -> 
    yatsy_ts:run().

clean() ->
    yatsy_ts:clean().

clean_and_run() ->
   yatsy_ts:clean_and_run().

%%%
%%% Report Generating functions
%%%
top(Url) ->
    yatsy_rg:top(Url).

app(Url, App) ->
    yatsy_rg:app(Url, App).

suite(Url, App, Suite) ->
    yatsy_rg:suite(Url, App, Suite).

tc(Url, App, Suite, Tc) ->
    yatsy_rg:tc(Url, App, Suite, Tc).



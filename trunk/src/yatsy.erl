%%%-------------------------------------------------------------------
%%% Created :  4 Sep 2006 by Torbjorn Tornkvist <tobbe@sej.hq.kred>
%%% Descr.  : (yatsy) Yet Another Test Server - Yaws compatible 
%%%
%%% @copyright 2006 Torbjörn Törnkvist
%%% @author Torbjörn Törnkvist <tobbe@tornkvist.org>
%%% @doc YATSY: Yet Another Test Server - Yaws compatible 
%%%     
%%% Yatsy is a simple test framework for testing your Erlang code.
%%% It makes it possible to execute test suites according to a
%%% defined structure and format, presenting the result either
%%% dynamically (via Yaws) or by generated HTML files.
%%%
%%% A testcase is implemented by a function that you write.
%%% You collect a number of such testcases in an Erlang module
%%% which ends in: <b>_SUITE.erl</b>. You may have any number
%%% of such test suite modules which you compile to beam files
%%% located in your <i>ebin</i> directories.
%%%
%%% You start your system, add the code paths to Yatsy and Yaws
%%% and start Yatsy. Yatsy will now automatically find all test
%%% suite modules that exist as beam files in you code path.
%%% Next you order Yatsy to run your test cases, suite by suite.
%%% The result will be accessible by pointing a Web browser to:
%%% <i>http://localhost:8888/yatsy.yaws</i> .
%%%
%%% It is possible to control the behaviour of Yatsy. This can
%%% be done either via a list of {Key,Value} tuples, which is
%%% provided at startup, or via shell environment variables.
%%% The latter method makes it easy to write wrapper scripts
%%% that can start your system and Yatsy. 
%%%
%%% Checklist, what you need to do:
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
%%%   </ol>
%%% 
%%%
%%%
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
    yaws:start_embedded(DocRoot, SL, GL).

    
%%%
%%% Yatsy Test Server interface
%%%
quick() ->
    quick([]).

quick(Config) ->
    %% Spawn us free from shell etc...
    spawn(fun() ->
		  yatsy_ts:start(Config),
		  wait_for_yatsy_ts(),
		  yatsy_rg:start(),
		  sleep(100),
		  start_yaws(),
		  sleep(100),
		  yatsy_ts:run()
	  end).

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
    yatsy_rg:start(),
    sleep(100),
    start_yaws().

start_link() ->
    start_link([]).

start_link(Config) ->
    yatsy_ts:start_link(Config),
    yatsy_rg:start_link(),
    sleep(100),
    start_yaws().

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



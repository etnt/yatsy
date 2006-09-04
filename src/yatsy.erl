%%%-------------------------------------------------------------------
%%% Created :  4 Sep 2006 by Torbjorn Tornkvist <tobbe@sej.hq.kred>
%%% Descr.  : (yatsy) Yet Another Test Server - Yaws compatible 
%%%-------------------------------------------------------------------
-module(yatsy).

-export([start/0, start_link/0, start/1, start_link/1,
	 run/0, clean/0, clean_and_run/0,
	 top/1, app/2, suite/3, tc/4
	 ]).

-export([test/0]).

test() ->
    yatsy_ts:test(),
    yatsy_rg:start(),
    yatsy_ts:run().


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



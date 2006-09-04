%%%-------------------------------------------------------------------
%%% File    : ybed.erl
%%% Descr.  : Small embedded yaws example
%%%
%%%           Make sure to change the docroot below to suit 
%%%           your situation.    
%%%
%%% Compile: erlc -I <path-to-Yaws-include-dir> -o ../ebin ybed.erl
%%%
%%%-------------------------------------------------------------------
-module(ybed).
-export([start/0]).

-define(DOCROOT, "/home/tobbe/yatsy/priv/docroot").

-include("yaws.hrl").

start() ->
    setup_embedded_yaws(),
    application:start(yaws),
    {ok, Host} = inet:gethostname(),
    GC = yaws_config:make_default_gconf(false, ""),
    SC = #sconf{port = 8888,
		servername = Host,
		listen = {0,0,0,0},
		docroot = ?DOCROOT},
    yaws_api:setconf(GC, [[SC]]).


setup_embedded_yaws() ->
    ok = application:load(yaws),
    ok = application:set_env(yaws, embedded, true).



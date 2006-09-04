%%%-------------------------------------------------------------------
%%% Created :  2 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : Test Case executor
%%%-------------------------------------------------------------------
-module(yatsy_tc).

-export([run/4,
	 suite_doc_and_load/2,
	 suite_tc/2
	]).

-import(yatsy_ts, [l2a/1, a2l/1]).

-include("yatsy_ts.hrl").


%%%
%%% Get the names of the Test Cases in the Test Suite.
%%%
suite_tc(Node, Mod) -> 
    Self = self(),
    spawn(fun() -> do_suite_tc(Self, Node, Mod, all, [suite]) end).

do_suite_tc(_Pid, Node, Mod, Fun, Args) ->
    case call(Node, Mod, Fun, Args) of
	Ts when list(Ts) -> 
	    Res = [get_tc_doc(Node, Mod, #tc{name = Tname}) || Tname <- Ts],
	    yatsy_ts:suite_tc_reply({ok, Res});
	Else -> 
	    yatsy_ts:suite_tc_reply({error, Else})
    end.
    
get_tc_doc(Node, Mod, #tc{name = Tname} = TC) ->
    case call(Node, Mod, Tname, [doc]) of
	Doc when list(Doc) -> TC#tc{doc = Doc};
	_                  -> TC   % ignore the doc string
    end.

%%%
%%% Check if the module can be loaded and then
%%% get the documentation for this suite.
%%%
suite_doc_and_load(Node, Mod) -> 
    Self = self(),
    spawn(fun() -> do_suite_doc_and_load(Self, Node, Mod, all, [doc]) end).

do_suite_doc_and_load(_Pid, Node, Mod, Fun, Args) ->
    call(Node, code, purge, [Mod]),
    case call(Node, code, load_file, [Mod]) of
	{module, Mod} ->
	    case call(Node, Mod, Fun, Args) of
		[Str] when list(Str) -> yatsy_ts:suite_doc_reply({ok, Str});
		Else                 -> yatsy_ts:suite_doc_reply({error, Else})
	    end;
	_ ->
	    Emsg = "(yatsy) failed to load module: "++a2l(Mod),
	    yatsy_ts:suite_doc_reply({error, Emsg})
    end.
	
    

%%%
%%% Run a Test Case
%%%
run(Node, Mod, TC, Config) ->
    Self = self(),
    spawn(fun() -> do_run(Self, Node, Mod, TC, Config) end).


do_run(_Pid, Node, Mod, #tc{name = Fun}, Config) ->
    case call(Node, Mod, init_per_testcase, [Fun, Config]) of
	NewConfig when list(NewConfig) ->
	    case call(Node, Mod, Fun, [NewConfig]) of
		true -> 
		    call(Node, Mod, fin_per_testcase, [Fun, NewConfig]),
		    yatsy_ts:tc_run_reply(ok);
		Else -> 
		    call(Node, Mod, fin_per_testcase, [Fun, NewConfig]),
		    yatsy_ts:tc_run_reply({error, Else})
	    end;
	Else ->
	    yatsy_ts:tc_run_reply({error, {init_per_testcase, Else}})
    end.
    

call(false, Mod, Fun, Args) -> local_call(Mod, Fun, Args);
call(Node, Mod, Fun, Args)  -> do_rpc(Node, Mod, Fun, Args).


local_call(Mod, Fun, Args) ->
    case catch apply(l2a(Mod), l2a(Fun), Args) of
	{'EXIT', Reason} ->
	    Loc = get_loc(),
	    {yatsy_exit, Loc, Reason};
	false ->
	    Loc = get_loc(),
	    {failed, Loc};
	true ->
	    true;
	Else ->
	    Else      % to make 'init_per_testcase' work
    end.
	
%%%
%%% Location: {Module, Line} , put by the ?line macro
%%%
get_loc() ->
    case get(yatsy_loc) of
	{_Mod, _Line} = Res -> Res;
	_                   -> false
    end.



do_rpc(Node, Mod, Fun, Args) -> 
    rpc:call(Node, l2a(Mod), l2a(Fun), Args).















    


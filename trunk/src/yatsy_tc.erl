%%%-------------------------------------------------------------------
%%% Created :  2 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : Test Case executor
%%%-------------------------------------------------------------------
-module(yatsy_tc).

-export([run/4,
	 suite_doc/2,
	 suite_tc/2
	]).

-include("../include/yatsy.hrl").


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
%%% Get the documentation for this suite.
%%%
suite_doc(Node, Mod) -> 
    Self = self(),
    spawn(fun() -> do_suite_doc(Self, Node, Mod, all, [doc]) end).

do_suite_doc(_Pid, Node, Mod, Fun, Args) ->
    case call(Node, Mod, Fun, Args) of
	[Str] when list(Str) -> yatsy_ts:suite_doc_reply({ok, Str});
	Else                 -> yatsy_ts:suite_doc_reply({error, Else})
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
	    yatsy_ts:tc_run_reply({error, Else})
    end.
    

call(false, Mod, Fun, Args) -> catch apply(l2a(Mod), l2a(Fun), Args);
call(Node, Mod, Fun, Args)  -> do_rpc(Node, Mod, Fun, Args).

do_rpc(Node, Mod, Fun, Args) -> tbd.   % FIXME


l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.










    


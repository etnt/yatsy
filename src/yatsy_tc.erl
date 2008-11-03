%%%-------------------------------------------------------------------
%%% Created :  2 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : Test Case executor
%%%-------------------------------------------------------------------
-module(yatsy_tc).

-export([run/4,
	 suite_doc_and_load/2,
	 suite_init/3,
	 suite_fin/3,
	 suite_tc/3,
	 local_call/4
	]).

%% Internal export
-export([call_fun/1]).

-import(yatsy_ts, [l2a/1, a2l/1]).

-include("yatsy_ts.hrl").


%%%
%%% Get the names of the Test Cases in the Test Suite.
%%%
suite_tc(Node, Mod, Conf) -> 
    Self = self(),
    {value, {test_senario, Str}} = lists:keysearch(test_senario, 1, Conf),
    Senario = x_to_atom(Str),
    spawn(fun() -> do_suite_tc(Self, Node, Mod, all, [Senario]) end).

x_to_atom(Str) when is_list(Str) -> list_to_atom(Str);
x_to_atom(Atom) -> Atom.

do_suite_tc(_Pid, Node, Mod, Fun, Args) ->
    case catch call(Node, Mod, Fun, Args) of
	{_, Ts} when list(Ts) -> 
	    Res = [get_tc_doc(Node, Mod, #tc{name = Tname}) || Tname <- Ts],
	    yatsy_ts:suite_tc_reply({ok, Res});
	Else -> 
	    yatsy_ts:suite_tc_reply({error, Else})
    end.
    
get_tc_doc(Node, Mod, #tc{name = Tname} = TC) ->
    case call(Node, Mod, Tname, [doc]) of
	{_, Doc} when list(Doc) -> TC#tc{doc = Doc};
	_                       -> TC   % ignore the doc string
    end.

%%%
%%% Check if the module can be loaded and then
%%% get the documentation for this suite.
%%%
suite_doc_and_load(Node, Mod) -> 
    Self = self(),
    spawn(fun() -> do_suite_doc_and_load(Self, Node, Mod, all, [doc]) end).

do_suite_doc_and_load(_Pid, Node, Mod0, Fun, Args) ->
    Mod = l2a(Mod0),
    call(Node, code, purge, [Mod]),
    case call(Node, code, load_file, [Mod]) of
	{_, {module, Mod}} ->
	    case call(Node, Mod, Fun, Args) of
		{_, [Str]} when list(Str) -> yatsy_ts:suite_doc_reply({ok, Str});
		_ -> yatsy_ts:suite_doc_reply({ok, "NO DOCUMENTATION AVAILABLE"})
	    end;
	_ ->
	    Emsg = "(yatsy) failed to load module: "++a2l(Mod),
	    yatsy_ts:suite_doc_reply({error, Emsg})
    end.
	
%%%
%%% Call the M:init_per_suite/1 function.
%%%
suite_init(Node, Mod, Config) -> 
    Self = self(),
    spawn(fun() -> do_suite_init(Self, Node, Mod, init_per_suite, [Config]) end).

do_suite_init(_Pid, Node, Mod, Fun, Args) ->
    case call(Node, Mod, Fun, Args) of
	{_, NewConf} when list(NewConf) -> yatsy_ts:suite_init_reply({ok, NewConf});
	Else                            -> yatsy_ts:suite_init_reply({error, Else})
    end.
	
%%%
%%% Call the M:fin_per_suite/1 function.
%%%
suite_fin(Node, Mod, Config) -> 
    Self = self(),
    spawn(fun() -> do_suite_fin(Self, Node, Mod, end_suite(Mod), [Config]) end).

do_suite_fin(_Pid, Node, Mod, Fun, Args) ->
    case call(Node, Mod, Fun, Args) of
	{_, ok}   -> yatsy_ts:suite_fin_reply(ok);
	Else      -> yatsy_ts:suite_fin_reply({error, Else})
    end.
	
    

%%%
%%% Run a Test Case
%%%
run(Node, Mod, TC, Config) ->
    Self = self(),
    spawn(fun() -> do_run(Self, Node, Mod, TC, Config) end).


do_run(_Pid, Node, Mod, #tc{name = Fun}, Config) ->
    case call(Node, Mod, init_per_testcase, [Fun, Config]) of
	{_, NewConfig} when list(NewConfig) ->
	    new_timeout(NewConfig),
	    case call(NewConfig, Node, Mod, Fun, [NewConfig]) of
		{Time, Res} when Res == true; Res == ok -> 
                    case run_fin(NewConfig) of
                        true -> 
                            call(Node, Mod, end_tc(Mod), [Fun, NewConfig]);
                        false ->
                            false
                    end,
		    yatsy_ts:tc_run_reply({ok, Time});
		Else -> 
                    case run_fin(NewConfig) of
                        true -> 
                            call(Node, Mod, end_tc(Mod), [Fun, NewConfig]);
                        false ->
                            false
                    end,
		    yatsy_ts:tc_run_reply({error, Else})
	    end;
	Else ->
	    yatsy_ts:tc_run_reply({error, {init_per_testcase, Else}})
    end.

run_fin(Config) ->
    case lists:keysearch(run_fin, 1, Config) of
        {value, {_, false}} -> false;
        _                   -> true
    end.
    
new_timeout(Config) ->
    case yatsy_ts:config(timeout, Config, false) of
	Timeout when integer(Timeout) ->
	    yatsy_ts:tc_new_timeout(Timeout);
	_ ->
	    false
    end.

call(Node, Mod, Fun, Args) -> 
    call([], Node, Mod, Fun, Args).

call(Config, false, Mod, Fun, Args) -> local_call(iact(Config), Mod, Fun, Args);
call(Config, Node, Mod, Fun, Args)  ->
    case yatsy_ts:config(call_in_yatsy_node, Config, false) of
	F when function(F) ->
	    local_call(iact(Config), ?MODULE, call_fun, [F]);
	_ ->
	    do_rpc(iact(Config), Node, Mod, Fun, Args)
    end.

call_fun(F) -> F().


iact(Config) -> 
    yatsy_ts:config(interactive, Config, false).

local_call(Iact, Mod, Fun, Args) ->
    if (Iact == false) -> % Yatsy is running in interactive mode ?
	    group_leader(whereis(init), self());
       true ->
	    true
    end,
    case catch timer:tc(l2a(Mod), l2a(Fun), Args) of
	{_Time, {'EXIT', Reason}} ->
	    Loc = get_loc(),
	    {yatsy_exit, Loc, Reason};
	{_Time, false} ->
	    Loc = get_loc(),
	    {failed, Loc};
	Else ->
	    Else
    end.
	
%%%
%%% Location: {Module, Line} , put by the ?line macro
%%%
get_loc() ->
    case get(yatsy_loc) of
	{_Mod, _Line} = Res -> Res;
	_                   -> false
    end.


%%%
%%% FIXME should have a wrapper function so that we can
%%% pick up line numbers etc.
%%%
do_rpc(Iact, Node, Mod, Fun, Args) -> 
    %% rpc:call(Node, l2a(Mod), l2a(Fun), Args).
    rpc:call(Node, ?MODULE, local_call, [Iact, l2a(Mod), l2a(Fun), Args]).

end_suite(Mod) -> 
    Exps = (list_to_atom(Mod)):module_info(exports),
    [R] = [F || F <- [fin_per_suite,end_per_suite],lists:member({F,1},Exps)],
    R.

end_tc(Mod) -> 
    Exps = (list_to_atom(Mod)):module_info(exports),
    [R] = [F||F<-[fin_per_testcase,end_per_testcase],lists:member({F,2},Exps)],
    R.

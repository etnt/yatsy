%%%-------------------------------------------------------------------
%%% File    : yatsy_SUITE.erl
%%% Created :  2 Sep 2006 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Descr.  : Example of a SUITE file. 
%%%-------------------------------------------------------------------
-module(yatsy_SUITE).

-export([init_per_suite/1, fin_per_suite/1, 
	 init_per_testcase/2, fin_per_testcase/2]).

-include("yatsy.hrl").

%% Test server callback functions
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Special Yatsy return values:
%%
%%   {timeout, MilliSeconds}  -  Set overall timeout value for the test cases.
%%   
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) when list(Config) ->
    {ok, Config}.

%%--------------------------------------------------------------------
%% Function: fin_per_suite(Config) -> _
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
fin_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config) -> Config
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Initiation before each test case
%%
%% Special Yatsy return values:
%%
%%   {timeout, MilliSeconds}  -  Set timeout value for this test case.
%%
%%   {call_in_yatsy_node, Fun} - Call this Fun in the local Yatsy node
%%                               and not in the target node. This makes
%%                               it possible to test, e.g a communication API.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%% Description: Initiation before each test case
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: fin_per_testcase(TestCase, Config) -> _
%% Case - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after each test case
%%--------------------------------------------------------------------
fin_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all(Clause) -> TestCases
%% Clause - atom() - suite | doc
%% TestCases - [Case] 
%% Case - atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all(doc) -> 
    ["Describe the main purpose of this suite"];

all(suite) -> 
    [test_case_1, test_case_2].

%% Test cases starts here.
%%--------------------------------------------------------------------
test_case_1(doc) -> 
    ["Describe the main purpose of test case"];
test_case_1(Config) when is_list(Config) -> 
    ?line 3 = 3,
    true.

test_case_2(doc) -> 
    ["Describe the main purpose of test case"];
test_case_2(Config) when is_list(Config) -> 
    ?line 3 = 4,
    true.

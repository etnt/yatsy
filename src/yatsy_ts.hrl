-ifndef(YATSY_TS_HRL).
-define(YATSY_TS_HRL, true).

-define(ilog(X,Y), error_logger:info_msg("*ilog ~p:~p: " X,
					 [?MODULE, ?LINE | Y])).

-ifdef(debug).
-define(dlog(X,Y), ?ilog(X,Y)).
-else.
-define(dlog(X,Y), true).
-endif.

-define(DEFAULT_TIMEOUT, 10000).    % 10 sek

-define(EMSG_FAILED_SUITE_DOC, "(yatsy) failed to retrieve the Suite doc string").
-define(EMSG_NO_TC, "no Test Case found").

-record(tc, {
	  name,                   % name of test case (a function name)
	  doc = "",               % test case doc string
	  rc = false,             % false | true | error
	  error = "",             % string: error output (when rc == error)
	  time = -1               % running time
	 }).

-record(suite, {
	  name,                   % name of suite module (e.g: xxx_SUITE)
	  path,                   % path to dir of beam file
	  doc = false,            % suite doc string
	  init = false,           % have xxx_SUITE:init_per_suite/1 been running ?
	  fin = false,            % should xxx_SUITE:fin_per_suite/1 be called ?
	  config = [],            % suite specific configuration           
	  tcs_only = false,       % list of tc_names if only those should run
	  finished = [],          % list of #tc{}
	  current = false,
	  queue = false           % list of #tc{}
	 }).

-record(app, {
	  name,
	  finished = [],          % list of #suite{}
	  current = false,
	  queue = []              % list of #suite{}
	 }).


-define(YATSY_IDLE,     idle).
-define(YATSY_RUNNING,  running).
-define(YATSY_FINISHED, finished).
-define(YATSY_ERROR,    error).

-endif.

-ifndef(YATSY_TS_HRL).
-define(YATSY_TS_HRL, true).

-define(DEFAULT_TIMEOUT, 10000).    % 10 sek

-define(EMSG_FAILED_SUITE_DOC, "(yatsy) failed to retrieve the Suite doc string").
-define(EMSG_NO_TC, "no Test Case found").

-record(tc, {
	  name,                   % name of test case (a function name)
	  doc = "",               % test case doc string
	  rc = false,             % false | true | error
	  error = ""              % string: error output (when rc == error)
	 }).

-record(suite, {
	  name,                   % name of suite module (e.g: xxx_SUITE)
	  path,                   % path to dir of beam file
	  doc = false,            % suite doc string
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
-define(YATSY_ERROR,    error).

-endif.

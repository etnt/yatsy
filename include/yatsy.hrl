-ifndef(YATSY_HRL).
-define(YATSY_HRL, true).

-ifdef(line_trace).
-line_trace(true).
-define(line,
	put(yatsy_loc,{?MODULE,?LINE}),
	io:format(lists:concat([?MODULE,",",integer_to_list(?LINE),": ~p"]),
		  [erlang:now()]),).
-else.
-define(line,put(yatsy_loc,{?MODULE,?LINE}),).
-endif.

-endif.

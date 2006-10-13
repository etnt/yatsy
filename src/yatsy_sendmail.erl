%%% File    : smtp.erl
%%% Author  :  <klacke@hyber.org>
%%% Description : send mail using local sendmail
%%% Created : 25 Jan 2005 by  <klacke@hyber.org>

-module(yatsy_sendmail).
-export([send/4]).

send(To, From, Subject, Text) ->
    Data = list_to_binary(["To: ", To,"\n",
	    "From: ", From, "\n"
	    "Subject: ", Subject,"\n",
	    "\n\n",
	    Text]),
    P = open_port({spawn, "/usr/sbin/sendmail -bm " ++ To ++ 
		   " 2> /tmp/mail.err" }, 
		  [stream, eof]),
    P ! {self(), {command, Data}},
    P ! {self(), close},
    rec_data(P, To),
    P ! {self(), close},
    case file:read_file("/tmp/mail.err") of
	{ok, Bin} when size(Bin) > 0 ->
	    error_logger:format("Output from mail: ~s~n",
				[binary_to_list(Bin)]),
	    file:delete("/tmp/mail.err");
	_ ->
	    file:delete("/tmp/mail.err"),
	    ok
    end.

rec_data(P, User) ->
    receive
	{P, {data, _Data}} ->
	    rec_data(P, User);
	{P, closed} ->
	    ok;
	{P, eof} ->
	    ok
    after 15000 ->
	    ok
    end.
		

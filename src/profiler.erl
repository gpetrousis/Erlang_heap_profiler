-module(profiler).
-export([start/0, stop/0, polling_start/0, polling_stop/1]).

-define(interval, 1000).
-define(threshold, 150).

polling_start() ->
	Pid = spawn(fun () -> small_process({0, 0}) end),
	Poll_id = spawn(fun () -> polling_start([], Pid) end),
	dbg:tracer(port, dbg:trace_port(file, "/tmp/trace.dmp")),
	dbg:p(processes, [garbage_collection, monotonic_timestamp]),
	Poll_id.

polling_start(L, Pid) -> 
	Procs = erlang:processes() -- [self()],
	Result_unfiltered = [poll_func(X, Pid) || X <- Procs],
	Pid ! {reset, self()},
	receive
		{respond, Ohs, Hs} ->
			Small_result = [[{<<"pid">>, <<"Small Processes">>}, 
						 {<<"type">>, <<"poll">>},
						 {<<"old_heap_size">>, Ohs},
						 {<<"heap_size">>, Hs},
						 {<<"timestamp">>, erlang:convert_time_unit(erlang:monotonic_time(), native, millisecond) + erlang:time_offset(millisecond)}]]
	end,
	Result = (Result_unfiltered -- [[]]) ++ Small_result,
	receive
		stop -> 
			dbg:stop_clear(),
			Dbg_pid = dbg:trace_client(file, "/tmp/trace.dmp", {fun handler/2, {L ++ Result, Pid}}),
			monitor(process, Dbg_pid),
			receive
				M -> M
			end
	after 
		?interval -> 
			polling_start(Result++L, Pid)
	end.

polling_stop(Pid) ->
	Pid ! stop,
	monitor(process, Pid),
	receive
		M -> M
	end
	ok.

poll_func(Pid, Small_pid) ->
	case erlang:process_info(Pid, garbage_collection_info) of
		undefined ->
			[];
		{_,Data} ->
			{_, _, Ohs, Hs} = parse_trace(Data),
			if
				(Ohs < ?threshold) and (Hs < ?threshold) ->
					Small_pid ! {add, Ohs, Hs},
					[];
				true ->
				[{<<"pid">>, list_to_binary(pid_to_list(Pid))}, 
				 {<<"type">>, <<"poll">>},
				 {<<"old_heap_size">>, Ohs},
				 {<<"heap_size">>, Hs},
				 {<<"timestamp">>, erlang:convert_time_unit(erlang:monotonic_time(), native, millisecond) + erlang:time_offset(millisecond)}]
			end
	end.


start() ->
	dbg:tracer(port, dbg:trace_port(file, "/tmp/trace.dmp")),
	dbg:p(processes, [garbage_collection, monotonic_timestamp]),
	ok.

stop() ->
	dbg:stop_clear(),
	Pid = dbg:trace_client(file, "/tmp/trace.dmp", {fun handler/2, []}),
        erlang:monitor(process, Pid),
        receive M -> M end,
        ok.

small_process({A,B}) ->
	receive
		{add, Ohs, Hs} -> 
			small_process({A+Ohs, B+Hs});
		{reset, Receiver} -> 
			Receiver ! {respond, A, B},
			small_process({0, 0});
		exit ->
			ok
	end.


handler(end_of_trace, {Return, Pid}) ->
	Pid ! {reset, self()},
	receive
		{respond, Ohs, Hs} ->
			Small_result = [[{<<"pid">>, <<"Small Processes">>}, 
						 {<<"type">>, <<"poll">>},
						 {<<"old_heap_size">>, Ohs},
						 {<<"heap_size">>, Hs},
						 {<<"timestamp">>, erlang:convert_time_unit(erlang:monotonic_time(), native, millisecond) + erlang:time_offset(millisecond)}]]
	end,
	Unsorted = [X || X <- (Return ++ Small_result), X /= []],
	Sorted = lists:sort(fun ([_,_,_,_,{<<"timestamp">>,X}], [_,_,_,_,{<<"timestamp">>,Y}]) -> X < Y end, Unsorted),
	Output = jsx:prettify(jsx:encode(Sorted)),
	file:write_file("dump.json", Output);
handler(M, {Return, Pid}) ->
	{Return ++ parse(M, Pid), Pid}.

parse_trace(L) -> parse_trace(L, {0,0,0,0}).

parse_trace([], {Ohbs, Hbs, Ohs, Hs}) -> {Ohbs, Hbs, Ohs, Hs};
parse_trace([X|Xs], {Ohbs, Hbs, Ohs, Hs}) ->
	case X of
		{old_heap_block_size, Y} -> parse_trace(Xs, {Y, Hbs, Ohs, Hs});
		{heap_block_size, Y} -> parse_trace(Xs, {Ohbs, Y, Ohs, Hs});
		{old_heap_size, Y} -> parse_trace(Xs, {Ohbs, Hbs, Y, Hs});
		{heap_size, Y} -> parse_trace(Xs, {Ohbs, Hbs, Ohs, Y});
		_ -> parse_trace(Xs, {Ohbs, Hbs, Ohs, Hs})
	end.



parse({trace_ts, Pid, _, L, Timestamp}, Small_pid) ->
	{_, _, Ohs, Hs} = parse_trace(L),
	if
		(Ohs > ?threshold) and (Hs > ?threshold) ->
			[[{<<"pid">>, list_to_binary(pid_to_list(Pid))},
			 {<<"type">>, <<"minor">>},
			 {<<"old_heap_size">>, Ohs},
			 {<<"heap_size">>, Hs},
			 {<<"timestamp">>, erlang:convert_time_unit(Timestamp, native, millisecond) + erlang:time_offset(millisecond)}]
			];
		true ->
			Small_pid ! {add, Ohs, Hs},
			[]
	end.


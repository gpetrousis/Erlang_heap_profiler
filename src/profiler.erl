-module(profiler).
-export([start/0, stop/0, polling_start/0, polling_stop/1]).

-define(interval, 5).

polling_start() ->
	spawn(fun () ->
                      dbg:tracer(port, dbg:trace_port(file, "/tmp/trace.dmp")),
                      dbg:p(all, [garbage_collection, monotonic_timestamp]),
                      polling_start([])
              end).

polling_start(L) -> 
	Procs = erlang:processes() -- [self()],
	Result = lists:flatmap(fun poll_func/1,Procs),
	receive
		stop -> 
			dbg:stop_clear(),
			Pid = dbg:trace_client(file, "/tmp/trace.dmp",
                                               {fun handler/2, Result}),
                        erlang:monitor(process, Pid),
                        receive M -> M end
	after 
		?interval -> 
			polling_start(Result++L)
	end.

polling_stop(Pid) ->
	Pid ! stop,
        erlang:monitor(process, Pid),
        receive M -> M end,
	ok.

poll_func(Pid) ->
	case erlang:process_info(Pid, garbage_collection_info) of
            undefined -> [];
            {_,Data} ->
                {_, _, Ohs, Hs} = parse_trace(Data),
                [[{<<"pid">>, list_to_binary(pid_to_list(Pid))}, 
                  {<<"type">>, <<"poll">>},
                  {<<"old_heap_size">>, Ohs},
                  {<<"heap_size">>, Hs},
                  {<<"timestamp">>, erlang:convert_time_unit(
                                      erlang:monotonic_time(), native, millisecond)
                   + erlang:time_offset(millisecond)}]]
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

handler(end_of_trace, Return) ->
	Sorted = lists:sort(fun ([_,_,_,_,{<<"timestamp">>,X}], [_,_,_,_,{<<"timestamp">>,Y}]) -> X < Y end, Return),
	Output = jsx:prettify(jsx:encode(Sorted)),
	file:write_file("dump.json", Output);
handler(M, Return) ->
	Return ++ parse(M).

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



parse({trace_ts, Pid, _, L, Timestamp}) ->
	{_, _, Ohs, Hs} = parse_trace(L),
	[[{<<"pid">>, list_to_binary(pid_to_list(Pid))},
	 {<<"type">>, <<"minor">>},
	 {<<"old_heap_size">>, Ohs},
	 {<<"heap_size">>, Hs},
	 {<<"timestamp">>, erlang:convert_time_unit(Timestamp, native, millisecond) + erlang:time_offset(millisecond)}]
	].


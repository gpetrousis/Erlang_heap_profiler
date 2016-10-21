-module(profiler).
-export([profiler_start/0, profiler_stop/0]).

profiler_start() ->
	dbg:tracer(port, dbg:trace_port(file, "/tmp/trace.dmp")),
	dbg:p(processes, [garbage_collection, monotonic_timestamp]),
	ok.


profiler_stop() ->
	dbg:stop_clear(),
	Pid = dbg:trace_client(file, "/tmp/trace.dmp", {fun handler/2, []}),
	ok.

handler(end_of_trace, Return) -> 
	Output = jsx:encode(Return),
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



parse({trace_ts, Pid, Type, L, Timestamp}) ->
	{Ohbs, Hbs, Ohs, Hs} = parse_trace(L),
	[{{<<"pid">>, list_to_binary(pid_to_list(Pid))}, 
	 {<<"type">>, <<"minor">>},
	 {<<"old_heap_size">>, Ohs},
	 {<<"heap_size">>, Hs},
	 {<<"timestamp">>, erlang:convert_time_unit(Timestamp, native, millisecond) + erlang:time_offset(millisecond)}}
	].

-module(heap_profiler_handler).
-export([init/2]).

init(Req0, [json]) ->
    TestJson = <<"[
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 1 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 123, timestamp : 2 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 3 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 12342, timestamp : 4 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 5 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 12, timestamp : 6 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 7 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 123, timestamp : 8 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 9 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 12, timestamp : 10 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 12342, timestamp : 11 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 123, timestamp : 12 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 123, timestamp : 13 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 123, timestamp : 14 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 15 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 16 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 123, timestamp : 17 },
     { pid : \"<0.1.0>\", type : minor, heap_size : 1234, timestamp : 18 }
   ]">>,
	Req = cowboy_req:reply(200,
		#{<<"content-type">> => <<"application/json">>},
		TestJson,
		Req0),
	{ok, Req, json};
init(Req0, State) ->
	{ok, Bin} = file:read_file("priv/index.html"),
	Req = cowboy_req:reply(200,
		#{<<"content-type">> => <<"text/html">>},
		Bin,
		Req0),
	{ok, Req, State}.

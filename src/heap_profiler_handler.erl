-module(heap_profiler_handler).
-export([init/2]).

init(Req0, [json]) ->
    {ok, Bin} = file:read_file("dump.json"),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Bin,
        Req0),
    {ok, Req, json};
init(Req0, State) ->
    {ok, Bin} = file:read_file("priv/index.html"),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        Bin,
        Req0),
    {ok, Req, State}.

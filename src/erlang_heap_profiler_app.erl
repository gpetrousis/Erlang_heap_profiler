%%%-------------------------------------------------------------------
%% @doc erlang_heap_profiler public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_heap_profiler_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [ {"/data", heap_profiler_handler, [json]},
				{"/", heap_profiler_handler, ["index.html"]}]}
	]),
	{ok, _} = cowboy:start_clear(my_http_listener, 100,
		[{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
    erlang_heap_profiler_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

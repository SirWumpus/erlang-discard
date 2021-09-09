-module(discard).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(discard).

start(normal, []) ->
	discard_sup:start_link().

stop(_) ->
	ok.

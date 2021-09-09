-module(discard_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

-define(DEFAULT_PORT, 9009).
-define(DEFAULT_WORKERS, 5).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Port = application:get_env(discard, port, ?DEFAULT_PORT),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, once}, {packet, line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600}, [
        {socket, {discard_server, start_link, [ListenSocket]}, % pass the socket!
        temporary, 1000, worker, [discard_server]}
    ]}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

%% Start with N listeners so that many multiple connections can be
%% started at once, without serialization.  In best circumstances, a
%% process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners() ->
    Workers = application:get_env(discard, workers, ?DEFAULT_WORKERS),
    [start_socket() || _ <- lists:seq(1, Workers)],
    ok.

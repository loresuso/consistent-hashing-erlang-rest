-module(monitor).
-export([start/0, init_monitor/1]).
-import(central_node, [init/1]).

% start: called to normally start the server process
start() ->
	io:format("Monitor ~p: Starting the server ~n", [self()]),
	ServerPid = spawn(central_node, init, [[]]),
	spawn(?MODULE, init_monitor, [ServerPid]).

init_monitor(ServerPid) ->
	register(monitor, self()),
	monitor(process, ServerPid),
	monitor_loop([]).

% monitor_loop: parent of the server checks for running central node
monitor_loop(Servers) ->
	receive
		% This message is sent everytime the list of servers is updated, as a backup
		{serversListUpdated, NewServers} ->
			io:format("Monitor: update ~n"),
			monitor_loop(NewServers);
		{'DOWN', _, process, _, Reason} ->
			io:format("Monitor: Central Node died. Reason: ~p~nMonitor: Recovering state and restarting...~n", [Reason]),
			start_with_state(Servers),
			monitor_loop(Servers);
		{terminate} ->
			whereis(central_node) ! {terminate}
	end.

% start with state: called when server was detected to be crashed
start_with_state(Servers) ->
	io:format("Monitor: Starting the server ~n"),
	ServerPid = spawn(central_node, init, [Servers]),
	monitor(process, ServerPid).


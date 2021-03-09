-module(central_node).
-export([init/1]).
init(Servers) ->
	io:format("Central Node pid is: ~p~n", [self()]),
	register(central_node, self()),
	% in case Servers is not empty, new central node monitors existing data nodes,
	% but it needs new monitor references in its state
	NewServers = monitor_data_nodes(Servers, []),
	loop(NewServers).

% Servers: stores nodeid's, pids and monitor references of data nodes
loop(Servers) ->
	io:format("Central Node: Servers state -> ~p~n", [Servers]),
	receive
		% Requests from Access Node
		{ReqId, AccessNode, {insert, Key, Value}} ->
			insert_key(ReqId, AccessNode, Key, Value, Servers),
			loop(Servers);
		{ReqId, AccessNode, {get, Key}} ->
			get_key(ReqId, AccessNode, Key, Servers),
			loop(Servers);
		{ReqId, AccessNode, {delete, Key}} ->
			delete_key(ReqId, AccessNode, Key, Servers),
			loop(Servers);

		% Data Node operations
		{DataNode, {join}} ->
			io:format("Central Node: Received join request from ~w~n", [DataNode]),
			{NewServersAfterJoin, Successor, NewNodeId, RefMonitor} = join(DataNode, Servers),
			NewServers = wait_join_completed(NewServersAfterJoin, Successor, {NewNodeId, DataNode, RefMonitor}),
			loop(NewServers);
		{DataNode, {leave}} ->
			io:format("Central Node: Received leave request from ~w~n", [DataNode]),
			NewServers = leave(DataNode, Servers),
			loop(NewServers);

		{terminate} ->
			io:format("Central Node: shutting down the system~n"),
			lists:foreach(fun({_, DataNode, _}) -> DataNode ! {terminate} end, Servers),
			io:format("Central Node: trusted termination ~n", []),
			{ok, terminated};

		% since DataNodes are monitored by the central node, they will sent a message like this
		% when they go down for any reason. Central node will delete that server from its list.
		{'DOWN', _, process, DataNode, Reason} ->
			io:format("Central Node: EXIT signal received from node: ~p, Reason: ~p~n", [DataNode, Reason]),
			NewServers = lists:filter(fun({_, Y, _}) -> Y =/= DataNode end, Servers),
			monitor ! {serversListUpdated, NewServers},
			loop(NewServers);

		WrongMessage ->
			io:format("Central Node: wrong message ~p~n", [WrongMessage]),
			loop(Servers)
	end.

% ------------------------------------ CLIENT OPS --------------------------------------------------
insert_key(ReqId, AccessNode, Key, Value, Servers) ->
	HashedKey = binary:decode_unsigned(crypto:hash(sha256, Key), big),
	SuccessorPid = lookup(Servers, HashedKey),
	SuccessorPid ! {ReqId, AccessNode, {insert, HashedKey, Value}}.

get_key(ReqId, AccessNode, Key, Servers) ->
	HashedKey = binary:decode_unsigned(crypto:hash(sha256, Key), big),
	SuccessorPid = lookup(Servers, HashedKey),
	SuccessorPid ! {ReqId, AccessNode, {get, HashedKey}}.

delete_key(ReqId, AccessNode, Key, Servers) ->
	HashedKey = binary:decode_unsigned(crypto:hash(sha256, Key), big),
	SuccessorPid = lookup(Servers, HashedKey),
	SuccessorPid ! {ReqId, AccessNode, {delete, HashedKey}}.

% --------------------------------------------------------------------------------------------------

% --------------------------------- DATA NODE OPS --------------------------------------------------
join(DataNode, Servers) ->
	NewNodeId = create_node_id(Servers),
	% start monitoring new DataNode
	RefMonitor = monitor(process, DataNode),
	% add to the list of servers the new NodeId and its PID to let the server send it messages
	NewServersUnsorted = lists:append(Servers, [{NewNodeId, DataNode, RefMonitor}]),
	NewServers = lists:sort(fun({X, _, _}, {Y, _, _}) -> X < Y end, NewServersUnsorted),
	% update backup list of servers
	monitor ! {serversListUpdated, NewServers},
	if
		length(NewServers) == 1 -> % first joining node
			DataNode ! {granted, NewNodeId},
			{NewServers, first, NewNodeId, RefMonitor};
		length(NewServers) =/= 1 ->
			SuccessorPid = find_successor(Servers, NewNodeId),
			DataNode ! {granted, NewNodeId, SuccessorPid},
			{NewServers, SuccessorPid, NewNodeId, RefMonitor}
	end.

% --------------------------- private functions --------------------------------
monitor_data_nodes([], List) ->
	lists:sort(fun({X, _, _}, {Y, _, _}) -> X < Y end, List);
monitor_data_nodes([H|T], List) ->
	{NodeId, Pid, _} = H,
	RefMonitor = monitor(process, Pid),
	NewList = List ++ {NodeId, Pid, RefMonitor},
	monitor_data_nodes(T, NewList).

% also check special case: space key wrap
create_node_id(Servers) ->
	% create random list
	RandomList = [rand:uniform(10) || _ <- lists:seq(1, 1000)],
	NodeId = binary:decode_unsigned(crypto:hash(sha256, RandomList), big),
	% until a new unique NodeId is not generated, continue to generate new random IDs
	AlreadyExists = lists:any(fun({X, _, _}) -> X == NodeId end, Servers),
	if
		AlreadyExists == true ->	
			create_node_id(Servers);
		true ->
			NodeId
	end.

% find_successor is used both for servers and keys, because they share the same key-space
find_successor([H|T], NewNodeId) ->
	find_successor([H|T], NewNodeId, H).

find_successor([], _, {_, NodePid, _}) -> NodePid; % for sure is minimum, nobody has key higher than me
find_successor([{NodeId, DataNode, _} | T], NewNodeId, MinNode) ->
	if
		NodeId > NewNodeId -> DataNode;
		true -> find_successor(T, NewNodeId, MinNode)
	end.

% look up it's the same as find successor..
lookup(L, Key) -> find_successor(L, Key).

% NewNodeId and DataNode are passed to this function to avoid keeping it into Servers if it doesn't send joinCompleted
wait_join_completed(Servers, Successor, {NewNodeId, DataNode, RefMonitor}) ->
	receive
		% wait for this specific message. All the other messages are kept in the saved messages
		% queue, until a match occurs or timeout
		{DataNode, {joinCompleted}} ->
			if
				Successor =/= first ->
					Successor ! {deleteOldKeys},
					Servers;
				true -> Servers
			end
	after 10000 ->
		io:format("Central Node: not receiving join completed from joining node~n"),
		NewServers = lists:delete({NewNodeId, DataNode, RefMonitor}, Servers),
		NewServers % avoid a joining node to block forever the server if it doesn't send joinCompleted
	end.

leave(DataNode, Servers) ->
	if
		length(Servers) == 1 ->
			DataNode ! {notgranted, "cannot leave, only one data node in the system"},
			Servers;
		length(Servers) > 1 ->
			SuccessorPid = find_successor(Servers, DataNode),
			DataNode ! {granted, SuccessorPid},
			% stop monitoring the leaving node/process
			[{_, _, RefMonitor}] = lists:filter(fun({_, Y, _}) -> Y == DataNode end, Servers),
			demonitor(RefMonitor),
			% wait for leaving confirmation
			NewServers = wait_leave_completed(DataNode, Servers),
			NewServers;
		true ->
			io:format("Central Node: unpredicted leaving request~n"),
			Servers
	end.

wait_leave_completed(DataNode, Servers) ->
	receive
		{DataNode, {leaveCompleted}} ->
			[ToDelete] = lists:filter(fun({_, Y, _}) -> Y == DataNode end, Servers),
			NewServers = lists:delete(ToDelete, Servers),
			NewServers
	after 10000 ->
		io:format("Central Node: not receiving leave completed from leaving node ~n"),
		[ToDelete] = lists:filter(fun({_, Y, _}) -> Y == DataNode end, Servers),
		NewServers = lists:delete(ToDelete, Servers),
		NewServers
	end.

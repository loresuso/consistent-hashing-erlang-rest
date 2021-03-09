-module(data_node).
-export([start/1, join/1]).
-import(reader, [read_line_from_file/2]).

% spawning of the process that executes a data node
start(ContainerId) ->
	% connect the process executing the data node to the central node process (to handle exit signals in central node)
	% read the name of the container where the central node has been deployed
	CentralNodeLocation = read_line_from_file('central_node_location.txt', ContainerId),
	net_kernel:connect_node(CentralNodeLocation),
	DataPid = spawn(?MODULE, join, [CentralNodeLocation]),
	io:format("DataNode pid is: ~p~n", [DataPid]).

% just spawned node: send the join request to the central node
join(CentralNodeLocation) ->
	{central_node, CentralNodeLocation} ! {self(), {join}},
	{NodeId, Dict} = complete_join(CentralNodeLocation),
	if
		NodeId == error ->
			io:format("DataNode ~w: joining unexpected termination~n", [self()]);
		true ->
			loop(Dict, NodeId, CentralNodeLocation)
	end.

% receive the reply of the join request
complete_join(CentralNodeLocation) ->
	receive
		% not the first node, so it needs a part of the keys from the successor
		{granted, NodeId, SuccessorPid} ->
			io:format("DataNode ~w: received granted message, with successor~n", [self()]),
			io:format("   Successor: ~p~n   NewNodeId: ~p~n", [SuccessorPid, NodeId]),
			NewDict = ask_for_keys(SuccessorPid, NodeId, CentralNodeLocation),
			{NodeId, NewDict};
		% first node in the key space
		{granted, NodeId} ->
			% ensure to send join completed in this case, otherwise is sent at the ask_for_keys
			{central_node, CentralNodeLocation} ! {self(), {joinCompleted}},
			{NodeId, dict:new()};
		_ ->
			{error, error}
	end.

% data node loop
loop(Dict, NodeId, CentralNodeLocation) ->
	io:format("Data Node ~w: loop -> Dict lenght: ~w~n",[self(), length(dict:to_list(Dict))]),
	receive
		% successor that has to send keys to a new joining node
		{NewPredecessorPid, {ask, NewPredecessorId}} ->
			io:format("Data Node ~w: predecessor is asking for keys~n", [self()]),
			OldKeysList = send_keys(NewPredecessorPid, NewPredecessorId, NodeId, Dict),
			NewDict = delete_old_keys(OldKeysList, dict:to_list(Dict)),
			loop(NewDict, NodeId, CentralNodeLocation);

		% Client ops
		{ReqId, AccessNode, {insert, Key, Value}}  ->
			io:format("Data Node ~w: insert request arrived~n", [self()]),
			NewDict = insert_key_value(ReqId, AccessNode, Dict, Key, Value),
			loop(NewDict, NodeId, CentralNodeLocation);
		{ReqId, AccessNode, {get, Key}} ->
			io:format("Data Node ~w: get request arrived~n", [self()]),
			get_value_from_key(ReqId, AccessNode, Dict, Key),
			loop(Dict, NodeId, CentralNodeLocation);
		{ReqId, AccessNode, {delete, Key}} ->
			io:format("Data Node ~w: delete request arrived~n", [self()]),
			NewDict = delete_value_from_key(ReqId, AccessNode, Dict, Key),
			loop(NewDict, NodeId, CentralNodeLocation);

		{leave} ->
			io:format("Data Node ~w: leave request arrived~n", [self()]),
			Granted = leave(Dict, CentralNodeLocation),
			if
				Granted == true ->
					io:format("Data Node ~w: leaving...~n", [self()]);
				% if leaving was not granted, data node is in the loop again
				Granted == false ->
					loop(Dict, NodeId, CentralNodeLocation)
			end;
		{insertFromLeaving, RecvList} ->
			io:format("Data Node ~w: received list from leaving node, length is: ~p~n", [self(), length(RecvList)]),
			RecvDict = dict:from_list(RecvList),
			NewDict = dict:merge(fun({_, Y, _}) -> Y end, Dict, RecvDict),
			loop(NewDict, NodeId, CentralNodeLocation);
		{terminate} ->
			io:format("Data Node ~w: trusted termination~n", [self()]);
		WrongMessage ->
			io:format("Data Node ~w: Wrong message ~p~n", [self(), WrongMessage]),
			loop(Dict, NodeId, CentralNodeLocation)
	end.

% ------------------------------------ CLIENT OPS --------------------------------------------------
insert_key_value(ReqId, AccessNode, Dict, Key, Value) ->
	Founded = dict:find(Key, Dict),
	if
		% I found an already existing value for that key
		Founded =/= error ->
			AccessNode ! {ReqId, "already existing"},
			Dict;
		% Not found, I can insert now the key-value pair
		true ->
			NewDict = dict:store(Key, Value, Dict),
			AccessNode ! {ReqId, "ok"},
			%central_node ! {insert, Key, ok},
			NewDict
	end.

get_value_from_key(ReqId, AccessNode, Dict, Key) ->
	Founded = dict:find(Key, Dict),
	if
		Founded == error ->
			AccessNode ! {ReqId, "not found"};
			%central_node ! {get, Key, notfound};
		true ->
			{_, Value} = Founded,
			AccessNode ! {ReqId, Value}
			%central_node ! {get, Key, Value}
	end.

delete_value_from_key(ReqId, AccessNode, Dict, Key) ->
	Founded = dict:find(Key, Dict),
	if
		Founded == error ->
			AccessNode ! {ReqId, "not found"},
			%central_node ! {delete, Key, error},
			Dict;
		true ->
			NewDict = dict:erase(Key, Dict),
			AccessNode ! {ReqId, "ok"},
			%central_node ! {delete, Key, ok},
			NewDict
	end.

% --------------------------------------------------------------------------------------------------

% --------------------------- private functions --------------------------------
% function to ask for keys from a successor
% no need of Dict in input because it has en empty one
ask_for_keys(SuccessorPid, NewNodeId, CentralNodeLocation) ->
	SuccessorPid ! {self(), {ask, NewNodeId}},
	RefMonitor = monitor(process, SuccessorPid),
	receive
		% receive dictionary and say it to the central node
		{send, NewDict} ->
			{central_node, CentralNodeLocation} ! {self(), {joinCompleted}},
			demonitor(RefMonitor),
			NewDict;
		% if successor node crashes, return an empty dictionary
		{'DOWN', _, process, DataNode, Reason} ->
			io:format("Data Node ~w: EXIT signal received from node: ~p, Reason: ~w~n", [self(), DataNode, Reason]),
			io:format("Data Node ~w: request of keys failed, successor has creashed. Empty dictionary!~n", [self()]),
			{central_node, CentralNodeLocation} ! {self(), {joinCompleted}},
			dict:new()
	end.

% function to send a part of the key-value pairs to a joining predecessor
send_keys(NewPredecessorPid, NewPredecessorId, NodeId, Dict) ->
	% work with lists, easier!
	List = dict:to_list(Dict),

	Filtered = lists:filter(fun({X,_}) -> X =< NewPredecessorId end, List),
	ToSend = lists:append(lists:filter(fun({X,_}) -> X > NodeId end, List), Filtered), % also check keys greater than me for the minimum server nodeid

	% convert the list into a dictionary and send to the new data node
	DictToSend = dict:from_list(ToSend),
	NewPredecessorPid ! {send, DictToSend},
	redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId),
	ToSend.

% redirect queued operations to the mailbox of the new predecessor if the latter is the new owner
redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId) ->
	receive
		{ReqId, AccessNode, {insert, Key, Value}} when Key =< NewPredecessorId ->
			NewPredecessorPid ! {ReqId, AccessNode, {insert, Key, Value}},
			redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId);
		{ReqId, AccessNode, {get, Key}} when Key =< NewPredecessorId ->
			NewPredecessorPid ! {ReqId, AccessNode, {get, Key}},
			redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId);
		{ReqId, AccessNode, {delete, Key}} when Key =< NewPredecessorId ->
			NewPredecessorPid ! {ReqId, AccessNode, {delete, Key}},
			redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId);
		{ReqId, AccessNode, {insert, Key, Value}} when  Key > NodeId ->
			NewPredecessorPid ! {ReqId, AccessNode, {insert, Key, Value}},
			redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId);
		{ReqId, AccessNode, {get, Key}} when Key > NodeId ->
			NewPredecessorPid ! {ReqId, AccessNode, {get, Key}},
			redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId);
		{ReqId, AccessNode, {delete, Key}} when Key > NodeId ->
			NewPredecessorPid ! {ReqId, AccessNode, {delete, Key}},
			redirect_ops(NewPredecessorPid, NewPredecessorId, NodeId)
	after 0 ->
		io:format("Data Node ~w: mailbox completely scanned~n", [self()])
	end.

	% keep scanning the mailbox until is empty, then the timer will be triggered
	% and it will return to the caller


% function to delete old keys, just sent, from a node
delete_old_keys(OldKeysList, DictList) ->
	% receive the ack from the central node
	receive
		{deleteOldKeys} ->
			% work with lists, easier!
			NewList = lists:subtract(DictList, OldKeysList),
			% convert the list into a dictionary and return it
			dict:from_list(NewList)
		after 1000000 ->
			io:format("Data Node ~w: I'm forced to delete sent keys, central node not responding~n", [self()]),
			NewList = lists:subtract(DictList, OldKeysList),
			dict:from_list(NewList)
	end.

leave(Dict, CentralNodeLocation) ->
	{central_node, CentralNodeLocation} ! {self(), {leave}},
	receive
		{granted, SuccessorPid} ->
			SuccessorPid ! {insertFromLeaving, dict:to_list(Dict)},
			redirect_ops_from_leaving(SuccessorPid),
			{central_node, CentralNodeLocation} ! {self(), {leaveCompleted}},
			true;
		{notgranted, Reason} ->
			io:format("Data Node ~w: Impossible to leave. Reason: ~s~n", [self(), Reason]),
			false
		after 10000000 ->
			io:format("Data Node ~w: Impossible to leave. Reason: central node not responding~n", [self()]),
			false
	end.

% all the queued ops must go to its successor
redirect_ops_from_leaving(SuccessorPid) ->
	receive
		Any ->
			SuccessorPid ! Any,
			redirect_ops_from_leaving(SuccessorPid)
	after 0 ->
		io:format("Data Node ~w: Sent all queued ops on leaving~n", [self()])
	end.

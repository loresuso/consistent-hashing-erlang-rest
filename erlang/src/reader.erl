-module(reader).
-export([read_line_from_file/2]).

% function interface, to be exported
read_line_from_file(File, LineNumber) ->
	{_, FileId} = file:open(File, [read]),
	read_line_from_file(FileId, nosuchline, LineNumber).

% base case, read the requested line from file
read_line_from_file(FileId, _, 0) ->
	{_, Readed} = file:read_line(FileId),
	NewReaded = lists:droplast(Readed),
	list_to_atom(NewReaded);
% recursive calls to get to the requested line
read_line_from_file(FileId, _, Iter) ->
	{_, NewReaded} = file:read_line(FileId),
	NewIter = Iter - 1,
	read_line_from_file(FileId, NewReaded, NewIter).

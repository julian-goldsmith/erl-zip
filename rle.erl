-module(rle).
-export([encode/1]).
-export([expand_runs/1]).

-record(rle_run, {byte, count}).

gen_runs([HByte|TBytes], #rle_run{byte=CByte, count=CCount}, Acc) when HByte == CByte ->
	gen_runs(TBytes, #rle_run{byte=CByte, count=CCount + 1}, Acc);
gen_runs([HByte|TBytes], #rle_run{byte=CByte, count=CCount}, Acc) when HByte /= CByte ->
	gen_runs(TBytes, #rle_run{byte=HByte, count=1},
		 Acc ++ [#rle_run{byte=CByte, count=CCount}]);
gen_runs([], #rle_run{byte=CByte, count=CCount}, Acc) when CCount > 0 ->
	Acc ++ [#rle_run{byte=CByte, count=CCount}].

% Returns a list of binaries to merge.
encode_run(#rle_run{byte=CByte, count=CCount}) when CCount < 4 ->
	lists:duplicate(CCount, CByte);
encode_run(#rle_run{byte=CByte, count=CCount}) when CCount =< 259 ->
	lists:duplicate(4, CByte) ++ [binary:encode_unsigned(CCount - 4)];
encode_run(#rle_run{byte=CByte, count=CCount}) ->
	encode_run(#rle_run{byte=CByte, count=259})
		++ encode_run(#rle_run{byte=CByte, count=CCount-259}).

encode([HByte|TBytes]) ->
	Runs = gen_runs(TBytes, #rle_run{byte=HByte, count=1}, []),
	Binaries = lists:flatmap(fun encode_run/1, Runs),
	binary:list_to_bin(Binaries);
encode([]) -> [].


expand_run(#rle_run{byte=CByte, count=CCount}) -> lists:duplicate(CCount, CByte).

expand_runs(Runs) -> lists:flatmap(fun expand_run/1, Runs).

-module(rle).
-export([encode/1]).
-export([decode/1]).

-record(rle_run, {byte, count}).

gen_runs([HByte|TBytes], #rle_run{byte=CByte, count=CCount}, Acc) when HByte == CByte ->
	gen_runs(TBytes, #rle_run{byte=CByte, count=CCount + 1}, Acc);
gen_runs([HByte|TBytes], #rle_run{byte=CByte, count=CCount}, Acc) when HByte /= CByte ->
	gen_runs(TBytes, #rle_run{byte=HByte, count=1},
		 Acc ++ [#rle_run{byte=CByte, count=CCount}]);
gen_runs([], #rle_run{byte=CByte, count=CCount}, Acc) when CCount > 0 ->
	Acc ++ [#rle_run{byte=CByte, count=CCount}].

% Returns a list of bytes to merge into a binary.
encode_run(#rle_run{byte=CByte, count=CCount}) when CCount < 4 ->
	lists:duplicate(CCount, CByte);
encode_run(#rle_run{byte=CByte, count=CCount}) when CCount =< 259 ->
	lists:duplicate(4, CByte) ++ [CCount - 4];
encode_run(#rle_run{byte=CByte, count=CCount}) ->
	encode_run(#rle_run{byte=CByte, count=259})
		++ encode_run(#rle_run{byte=CByte, count=CCount-259}).

% TODO: It would be much more efficient to generate runs while encoding.
encode([HByte|TBytes]) ->
	Runs = gen_runs(TBytes, #rle_run{byte=HByte, count=1}, []),
	EncBytes = lists:flatmap(fun encode_run/1, Runs),
	binary:list_to_bin(EncBytes);
encode([]) -> [].


% TODO: It would be much more efficient to expand runs while decoding.
expand_run(#rle_run{byte=CByte, count=CCount}) -> lists:duplicate(CCount, CByte).

decode_runs(<<CByte, NByte, Rest/binary>>, #rle_run{byte=PByte, count=PCount}, Acc) when PCount == 4 ->
	decode_runs(Rest, #rle_run{byte=NByte, count=1}, Acc ++ [#rle_run{byte=PByte, count=(4 + CByte)}]);
decode_runs(<<CByte, Rest/binary>>, #rle_run{byte=PByte, count=PCount}, Acc) when CByte == PByte ->
	decode_runs(Rest, #rle_run{byte=PByte, count=PCount + 1}, Acc);
decode_runs(<<CByte, Rest/binary>>, Run, Acc) -> 
	decode_runs(Rest, #rle_run{byte=CByte, count=1}, Acc ++ [Run]);
decode_runs(<<>>, Run, Acc) -> Acc ++ [Run].

decode(<<HByte,TBytes/binary>>) ->
	Runs = decode_runs(TBytes, #rle_run{byte=HByte, count=1}, []),
	lists:flatmap(fun expand_run/1, Runs);
decode(<<>>) -> [].

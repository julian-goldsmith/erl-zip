-module(rle).
-export([encode/1]).
-export([decode/1]).

-record(rle_run, {byte, count}).

% Returns a list of bytes to merge into a binary.
encode_run(#rle_run{byte=CByte, count=CCount}) when CCount < 4 ->
	lists:duplicate(CCount, CByte);
encode_run(#rle_run{byte=CByte, count=CCount}) when CCount =< 259 ->
	lists:duplicate(4, CByte) ++ [<<(CCount - 4):8>>];
encode_run(#rle_run{byte=CByte, count=CCount}) ->
	lists:duplicate(4, CByte) ++ [<<255>>]
		++ encode_run(#rle_run{byte=CByte, count=CCount-259}).

encode_runs(<<HByte, TBytes/binary>>, #rle_run{byte=CByte, count=CCount}, Acc) when HByte == CByte ->
	encode_runs(TBytes, #rle_run{byte=CByte, count=CCount + 1}, Acc);
encode_runs(<<HByte, TBytes/binary>>, Run, Acc) ->
	encode_runs(TBytes, #rle_run{byte=HByte, count=1}, Acc ++ encode_run(Run));
encode_runs(<<>>, Run, Acc) -> Acc ++ encode_run(Run).

encode(<<HByte, TBytes/binary>>) ->
	EncBytes = encode_runs(TBytes, #rle_run{byte=HByte, count=1}, []),
	binary:list_to_bin(EncBytes);
encode([]) -> <<>>.


% PCount of 0 means we need to initialize the run.
decode_runs(<<CByte, TBytes/binary>>, _PByte, PCount, Acc) when PCount == 0 ->
	decode_runs(TBytes, CByte, 1, Acc);
decode_runs(<<CByte, TBytes/binary>>, PByte, PCount, Acc) when PCount == 4 ->
	decode_runs(TBytes, 0, 0, Acc ++ lists:duplicate(4 + CByte, PByte));
decode_runs(<<CByte, TBytes/binary>>, PByte, PCount, Acc) when CByte == PByte ->
	decode_runs(TBytes, PByte, PCount + 1, Acc);
decode_runs(<<CByte, TBytes/binary>>, PByte, PCount, Acc) -> 
	decode_runs(TBytes, CByte, 1, Acc ++ lists:duplicate(PCount, PByte));
decode_runs(<<>>, PByte, PCount, Acc) ->
	Acc ++ lists:duplicate(PCount, PByte).

decode(Bytes) ->
	DecBytes = decode_runs(Bytes, 0, 0, []),
	binary:list_to_bin(DecBytes).

-module(rle).
-export([encode/1]).
-export([decode/1]).

encode_run(CByte, CCount, Acc) when CCount < 4 ->
	Expanded = binary:copy(<<CByte>>, CCount),
	<<Acc/binary, Expanded/binary>>;
encode_run(CByte, CCount, Acc) when CCount =< 259 ->
	<<Acc/binary, CByte, CByte, CByte, CByte, (CCount - 4):8>>;
encode_run(CByte, CCount, Acc) ->
	encode_run(CByte, CCount - 259,
		<<Acc/binary, CByte, CByte, CByte, CByte, 255>>).

encode_runs(<<HByte, TBytes/binary>>, CByte, CCount, Acc) when HByte == CByte ->
	encode_runs(TBytes, CByte, CCount + 1, Acc);
encode_runs(<<HByte, TBytes/binary>>, CByte, CCount, Acc) ->
	encode_runs(TBytes, HByte, 1, encode_run(CByte, CCount, Acc));
encode_runs(<<>>, CByte, CCount, Acc) ->
	encode_run(CByte, CCount, Acc).

encode(<<HByte, TBytes/binary>>) -> encode_runs(TBytes, HByte, 1, <<>>);
encode([]) -> <<>>.


% PCount of 0 means we need to initialize the run.
% That is required because we don't always have a next byte.
decode_runs(<<CByte, TBytes/binary>>, PByte, PCount, Acc) when PCount == 4 ->
	Expanded = binary:copy(<<PByte>>, 4 + CByte),
	decode_runs(TBytes, 0, 0, <<Acc/binary, Expanded/binary>>);
decode_runs(<<CByte, TBytes/binary>>, PByte, PCount, Acc)
		when (CByte == PByte) or (PCount == 0) ->
	decode_runs(TBytes, CByte, PCount + 1, Acc);
decode_runs(<<CByte, TBytes/binary>>, PByte, PCount, Acc) -> 
	Expanded = binary:copy(<<PByte>>, PCount),
	decode_runs(TBytes, CByte, 1, <<Acc/binary, Expanded/binary>>);
decode_runs(<<>>, PByte, PCount, Acc) ->
	Expanded = binary:copy(<<PByte>>, PCount),
	<<Acc/binary, Expanded/binary>>.

decode(Bytes) -> decode_runs(Bytes, 0, 0, <<>>).

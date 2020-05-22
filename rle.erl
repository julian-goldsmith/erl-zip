-module(rle).
-export([encode/1]).
-export([decode/1]).

encode_run(CByte, CCount, Acc) when CCount >= 259 ->
	encode_run(CByte, CCount - 259,
		<<Acc/binary, CByte, CByte, CByte, CByte, 255>>);
encode_run(CByte, CCount, Acc) when CCount >= 4 ->
	<<Acc/binary, CByte, CByte, CByte, CByte, (CCount - 4)>>;
encode_run(CByte, CCount, Acc) ->
	Expanded = binary:copy(<<CByte>>, CCount),
	<<Acc/binary, Expanded/binary>>.

encode(<<HByte, TBytes/binary>>, HByte, CCount, Acc) ->
	encode(TBytes, HByte, CCount + 1, Acc);
encode(<<HByte, TBytes/binary>>, CByte, CCount, Acc) ->
	encode(TBytes, HByte, 1, encode_run(CByte, CCount, Acc));
encode(<<>>, CByte, CCount, Acc) ->
	encode_run(CByte, CCount, Acc).

encode(<<HByte, TBytes/binary>>) -> encode(TBytes, HByte, 1, <<>>);
encode(<<>>) -> <<>>.


% PCount of 0 means we need to initialize the run.
% That is required because we don't always have a next byte.
decode(<<CByte, TBytes/binary>>, _, 0, Acc) ->
	decode(TBytes, CByte, 1, Acc);
decode(<<CByte, TBytes/binary>>, PByte, 4, Acc) ->
	Expanded = binary:copy(<<PByte>>, 4 + CByte),
	decode(TBytes, 0, 0, <<Acc/binary, Expanded/binary>>);
decode(<<CByte, TBytes/binary>>, CByte, PCount, Acc) ->
	decode(TBytes, CByte, PCount + 1, Acc);
decode(<<CByte, TBytes/binary>>, PByte, PCount, Acc) -> 
	Expanded = binary:copy(<<PByte>>, PCount),
	decode(TBytes, CByte, 1, <<Acc/binary, Expanded/binary>>);
decode(<<>>, PByte, PCount, Acc) ->
	Expanded = binary:copy(<<PByte>>, PCount),
	<<Acc/binary, Expanded/binary>>.

decode(Bytes) -> decode(Bytes, 0, 0, <<>>).

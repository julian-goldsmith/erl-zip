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


% Manually matching is simpler and more concise than counting.
decode(<<CByte, CByte, CByte, CByte, CCount, TBytes/binary>>, Acc) ->
	Expanded = binary:copy(<<CByte>>, 4 + CCount),
	decode(TBytes, <<Acc/binary, Expanded/binary>>);
decode(<<CByte, CByte, CByte, TBytes/binary>>, Acc) ->
	decode(TBytes, <<Acc/binary, CByte, CByte, CByte>>);
decode(<<CByte, CByte, TBytes/binary>>, Acc) ->
	decode(TBytes, <<Acc/binary, CByte, CByte>>);
decode(<<CByte, TBytes/binary>>, Acc) ->
	decode(TBytes, <<Acc/binary, CByte>>);
decode(<<>>, Acc) -> Acc.

decode(Bytes) -> decode(Bytes, <<>>).

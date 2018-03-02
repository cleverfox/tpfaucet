-module(naddress).

-export([check/1, decode/1, encode/1]).

-export([construct_private/2, construct_public/3,
	 parse/1, splitby/2]).

-export([g2i/1, i2g/1]).

-export([mine/1]).

construct_public(Group, Block, Address)
    when Group < 65536, Block < 2097152,
	 Address < 16777216 ->
    binary:encode_unsigned(Address bor (Block bsl 24) bor
			     (Group bsl 45)
			     bor (4 bsl 61)).

construct_private(Block, Address)
    when Block < 137438953472, Address < 16777216 ->
    IntPart = Address bor (Block bsl 24),
    binary:encode_unsigned(IntPart bor (5 bsl 61)).

parse(<<X:64/big>>) -> parse(X);
parse(Int)
    when is_integer(Int) andalso
	   Int >= 9223372036854775808 andalso
	     Int < 13835058055282163712 ->
    case Int bsr 61 of
      4 ->
	  #{type => public, address => Int band 16777215,
	    block => (Int bsr 24) band 2097151,
	    group => (Int bsr 45) band 65535};
      5 ->
	  #{type => private, address => Int band 16777215,
	    block => (Int bsr 24) band 137438953471}
    end.

encode(<<X:64/big>>) -> encode(X);
encode(Int)
    when is_integer(Int) andalso
	   Int >= 9223372036854775808 andalso
	     Int < 13835058055282163712 ->
    Type = case Int bsr 61 of
	     4 -> public;
	     5 -> private
	   end,
    CSum = erlang:crc32(<<Int:64/big>>),
    case Type of
      private ->
	  iolist_to_binary(lists:flatten(io_lib:format("~16.16.0B~2.16.0B",
						       [Int band (1 bsl 61 - 1),
							CSum rem 256])));
      public ->
	  Digits = (1 bsl 45 - 1) band Int,
	  Prefix = Int bsr 45,
	  Group = Prefix band 65535,
	  iolist_to_binary([i2g(Group),
			    lists:flatten(io_lib:format("~14.10.0B", [Digits])),
			    io_lib:format("~2.10.0B", [CSum rem 100])])
    end.

check(UserAddr) ->
    try {true, parse(UserAddr)} catch
      _:_ -> {false, unknown}
    end.

decode(UserAddr) ->
    C = cleanup(UserAddr),
    case size(C) of
      20 ->
	  <<H4:4/binary, B14:14/binary, BCRC:2/binary>> = C,
	  IntPart = binary_to_integer(B14, 10),
	  CharPart = g2i(H4),
	  Address = IntPart bor (CharPart bsl 45) bor (4 bsl 61),
	  CSum = erlang:crc32(<<Address:64/big>>) rem 100,
	  CRC = binary_to_integer(BCRC),
	  if CSum == CRC -> binary:encode_unsigned(Address);
	     true -> throw({error, address_crc})
	  end;
      18 ->
	  PI = binary_to_integer(C, 16),
	  Address = PI bsr 8 bor (5 bsl 61),
	  CRC = PI band 255,
	  CSum = erlang:crc32(<<Address:64/big>>) band 255,
	  if CSum == CRC -> binary:encode_unsigned(Address);
	     true -> throw({error, address_crc})
	  end;
      _ -> throw(bad_addr)
    end.

splitby(String, N) when is_list(String) ->
    case String of
      [H1, H2, H3, H4 | Rest] ->
	  [H1, H2, H3, H4, " ", splitby(Rest, N)];
      _ -> String
    end;
splitby(String, N) when is_binary(String) ->
    case String of
      <<H1:N/binary, Rest/binary>> ->
	  <<H1/binary, " ", (splitby(Rest, N))/binary>>;
      _ -> String
    end.

binclean(<<>>) -> <<>>;
binclean(<<" ", Rest/binary>>) -> binclean(Rest);
binclean(<<I:8/integer, Rest/binary>>) ->
    <<I:8/integer, (binclean(Rest))/binary>>.

cleanup(Address) when is_binary(Address) ->
    binclean(Address);
cleanup(Address) when is_list(Address) ->
    list_to_binary(lists:filter(fun (32) -> false;
				    (_) -> true
				end,
				Address)).

bin2i(I) when I >= $a andalso $z >= I -> I - $a;
bin2i(I) when I >= $A andalso $Z >= I -> I - $A;
bin2i(_) -> throw({error, badchar}).

g2i(<<L1:8/integer, L2:8/integer, D:2/binary>>) ->
    bin2i(L1) * 2600 + bin2i(L2) * 100 +
      binary_to_integer(D).

i2g(I) when I < 65536 ->
    L0 = I rem 10,
    L1 = I div 10 rem 10,
    L2 = I div 100 rem 26,
    L3 = I div 2600,
    <<($A + L3), ($A + L2), ($0 + L1), ($0 + L0)>>.

mine(Num) ->
    Tail = Num div 100,
    CS = Num rem 100,
    lists:filtermap(fun (N) ->
			    B = <<4:3/big, N:16/big, Tail:45/big>>,
			    C = erlang:crc32(B),
			    if C rem 100 == CS -> {true, encode(B)};
			       true -> false
			    end
		    end,
		    lists:usort(lists:seq(0, 256) ++
				  lists:seq(65300, 65535) ++
				    [X * 64 + 1 || X <- lists:seq(0, 1024)])).

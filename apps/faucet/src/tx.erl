-module(tx).

-export([get_ext/2, pack/1, set_ext/3, sign/2, unpack/1,
	 verify/1]).

checkaddr(<<Ia:64/big>>) -> {new, {true, Ia}};
checkaddr(Address) -> {old, address:check(Address)}.

get_ext(K, Tx) ->
    Ed = maps:get(extdata, Tx, #{}),
    case maps:is_key(K, Ed) of
      true -> {ok, maps:get(K, Ed)};
      false -> undefined
    end.

set_ext(K, V, Tx) ->
    Ed = maps:get(extdata, Tx, #{}),
    Tx#{extdata => maps:put(K, V, Ed)}.

mkmsg(#{from := From, amount := Amount, cur := Currency,
	to := To, seq := Seq, timestamp := Timestamp} =
	  Tx) ->
    Append = maps:get(extradata, Tx, <<"">>),
    if is_binary(From) -> ok;
       true -> throw(non_bin_addr_from)
    end,
    if is_binary(To) -> ok;
       true -> throw(non_bin_addr_to)
    end,
    msgpack:pack(["tx", From, To, trunc(Amount),
		  if is_list(Currency) -> Currency;
		     is_binary(Currency) -> binary_to_list(Currency)
		  end,
		  Timestamp, Seq,
		  if is_list(Append) -> Append;
		     is_binary(Append) -> binary_to_list(Append)
		  end]);
mkmsg(Unknown) -> throw({unknown_tx_type, Unknown}).

sign(#{from := From} = Tx, PrivKey) ->
    Pub = tpecdsa:secp256k1_ec_pubkey_create(PrivKey, true),
    case checkaddr(From) of
      {new, {true, _IAddr}} -> ok;
      {old, {true, Fat}} ->
	  if Fat < 256 ->
		 NewFrom = address:pub2addr(Fat, Pub),
		 if NewFrom =/= From ->
			throw({invalid_key, mismatch_from_address});
		    true -> ok
		 end;
	     true -> error
	  end;
      _ -> throw({invalid_address, from})
    end,
    TxBin = mkmsg(Tx),
    {ok, [MType | LTx]} = msgpack:unpack(TxBin),
    Sig = tpecdsa:secp256k1_ecdsa_sign(TxBin, PrivKey,
				       default, <<>>),
    msgpack:pack(maps:merge(#{type => MType, tx => LTx,
			      sig =>
				  maps:put(Pub, Sig,
					   maps:get(signature, Tx, #{}))},
			    maps:with([extdata], Tx))).

verify(#{register := _, type := register} = Tx) ->
    {ok, Tx};
verify(#{from := From, sig := HSigs, timestamp := T} =
	   Tx) ->
    Message = mkmsg(Tx),
    if is_integer(T) -> ok;
       true -> throw({bad_timestamp, T})
    end,
    {Valid, Invalid} = case checkaddr(From) of
			 {new, {true, _IAddr}} ->
			     case ledger:get(From) of
			       #{pubkey := PK} when is_binary(PK) ->
				   maps:fold(fun (Pub, Sig,
						  {AValid, AInvalid}) ->
						     case
						       tpecdsa:secp256k1_ecdsa_verify(Message,
										      Sig,
										      Pub)
							 of
						       correct when PK == Pub ->
							   {AValid + 1,
							    AInvalid};
						       _ ->
							   {AValid,
							    AInvalid + 1}
						     end
					     end,
					     {0, 0}, HSigs);
			       _ -> throw({ledger_err, From})
			     end;
			 {old, {true, Fat}} ->
			     maps:fold(fun (Pub, Sig, {AValid, AInvalid}) ->
					       NewFrom = address:pub2addr(Fat,
									  Pub),
					       if NewFrom =/= From ->
						      {AValid, AInvalid + 1};
						  true ->
						      case
							tpecdsa:secp256k1_ecdsa_verify(Message,
										       Sig,
										       Pub)
							  of
							correct ->
							    {AValid + 1,
							     AInvalid};
							_ ->
							    {AValid,
							     AInvalid + 1}
						      end
					       end
				       end,
				       {0, 0}, HSigs);
			 _ -> throw({invalid_address, from})
		       end,
    case Valid of
      0 -> bad_sig;
      N when N > 0 ->
	  {ok,
	   Tx#{sigverify => #{valid => Valid, invalid => Invalid}}}
    end;
verify(Bin) when is_binary(Bin) ->
    Tx = unpack(Bin), verify(Tx).

pack(#{hash := _, header := _, sign := _} = Block) ->
    msgpack:pack(#{type => <<"block">>,
		   block => block:pack(Block)});
pack(#{patch := LPatch, sig := Sigs} = Tx) ->
    msgpack:pack(maps:merge(#{type => <<"patch">>,
			      patch => LPatch, sig => Sigs},
			    maps:with([extdata], Tx)));
pack(#{register := Reg} = Tx) ->
    msgpack:pack(maps:merge(maps:with([address], Tx),
			    #{type => <<"register">>, register => Reg}));
pack(#{sig := Sigs} = Tx) ->
    TxBin = mkmsg(Tx),
    {ok, [MType | LTx]} = msgpack:unpack(TxBin),
    msgpack:pack(maps:merge(#{type => MType, tx => LTx,
			      sig => Sigs},
			    maps:with([extdata], Tx)));
pack(Unknown) -> throw({unknown_tx_to_pack, Unknown}).

unpack(Tx) when is_map(Tx) -> Tx;
unpack(BinTx) when is_binary(BinTx) -> unpack_mp(BinTx).

unpack_mp(BinTx) when is_binary(BinTx) ->
    {ok, Tx0} = msgpack:unpack(BinTx,
			       [{known_atoms,
				 [type, sig, tx, patch, register, register,
				  address, block]},
				{unpack_str, as_binary}]),
    Tx = maps:fold(fun ("tx", Val, Acc) ->
			   maps:put(tx,
				    lists:map(fun (LI) when is_list(LI) ->
						      list_to_binary(LI);
						  (OI) -> OI
					      end,
					      Val),
				    Acc);
		       ("type", Val, Acc) ->
			   maps:put(type,
				    try erlang:list_to_existing_atom(Val) catch
				      error:badarg -> Val
				    end,
				    Acc);
		       ("address", Val, Acc) ->
			   maps:put(address, list_to_binary(Val), Acc);
		       ("register", Val, Acc) ->
			   maps:put(register, list_to_binary(Val), Acc);
		       ("sig", Val, Acc) ->
			   maps:put(sig,
				    if is_map(Val) ->
					   maps:fold(fun (PubK, PrivK, KAcc) ->
							     maps:put(iolist_to_binary(PubK),
								      iolist_to_binary(PrivK),
								      KAcc)
						     end,
						     #{}, Val);
				       is_list(Val) ->
					   lists:foldl(fun ([PubK, PrivK],
							    KAcc) ->
							       maps:put(PubK,
									PrivK,
									KAcc)
						       end,
						       #{}, Val)
				    end,
				    Acc);
		       (sig, Val, Acc) ->
			   maps:put(sig,
				    if is_map(Val) ->
					   maps:fold(fun (PubK, PrivK, KAcc) ->
							     maps:put(iolist_to_binary(PubK),
								      iolist_to_binary(PrivK),
								      KAcc)
						     end,
						     #{}, Val);
				       is_list(Val) ->
					   case Val of
					     [X | _] when is_binary(X) -> Val;
					     [[_, _] | _] ->
						 lists:foldl(fun ([PubK, PrivK],
								  KAcc) ->
								     maps:put(PubK,
									      PrivK,
									      KAcc)
							     end,
							     #{}, Val)
					   end;
				       Val == <<>> -> []
				    end,
				    Acc);
		       (K, Val, Acc) -> maps:put(K, Val, Acc)
		   end,
		   #{}, Tx0),
    #{type := Type} = Tx,
    R = case Type of
	  tx ->
	      #{sig := Sig} = Tx,
	      [From, To, Amount, Cur, Timestamp, Seq, ExtraJSON] =
		  maps:get(tx, Tx),
	      if is_integer(Timestamp) -> ok;
		 true -> throw({bad_timestamp, Timestamp})
	      end,
	      #{type => Type, from => From, to => To,
		amount => Amount, cur => Cur, timestamp => Timestamp,
		seq => Seq, extradata => ExtraJSON, sig => Sig};
	  block -> block:unpack(maps:get(block, Tx));
	  patch ->
	      #{sig := Sig} = Tx,
	      #{patch => maps:get(patch, Tx), sig => Sig};
	  register ->
	      PubKey = maps:get(register, Tx),
	      case size(PubKey) of
		33 -> ok;
		true -> throw(bad_pubkey)
	      end,
	      case maps:is_key(address, Tx) of
		false -> #{type => register, register => PubKey};
		true ->
		    #{type => register, register => PubKey,
		      address => maps:get(address, Tx)}
	      end;
	  _ -> throw({"bad tx type", Type})
	end,
    case maps:is_key(<<"extdata">>, Tx) of
      true -> R#{extdata => maps:get(<<"extdata">>, Tx)};
      false -> R
    end.

-module(mywallet).

-behaviour(gen_server).

-export([start_link/1]).

-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

start_link(Name) ->
    gen_server:start_link({local, mywallet}, mywallet,
			  [Name], []).

init([Name]) ->
    application:ensure_all_started(inets),
    self() ! refetch,
    {ok, #{name => Name, seq => undefined}}.

handle_call({give, Addr, Amount}, _From,
	    #{url := URL, addr := MyAddr, key := MyKey,
	      seq := Seq} =
		State) ->
    Tx = #{amount => Amount, cur => <<"FTT">>,
	   extradata =>
	       jsx:encode(#{message =>
				<<"To ", (naddress:encode(Addr))/binary,
				  " with love">>}),
	   from => MyAddr, to => Addr, seq => Seq + 1,
	   timestamp => os:system_time(millisecond)},
    try NewTx = tx:sign(Tx, MyKey),
	wf:info("TX ~p", [tx:unpack(NewTx)]),
	BinTX = base64:encode(NewTx),
	{ok, {{_, 200, _}, _, ResBody}} = httpc:request(post,
							{URL ++ "/api/tx/new",
							 [], "application/json",
							 <<"{\"tx\":\"",
							   BinTX/binary,
							   "\"}">>},
							[],
							[{body_format,
							  binary}]),
	Res = jsx:decode(ResBody, [return_maps]),
	wf:info("Res ~p", [Res]),
	case Res of
	  #{<<"result">> := <<"ok">>, <<"txid">> := TxID} ->
	      {reply,
	       {ok, TxID,
		maps:fold(fun (sig, _, Acc) -> Acc;
			      (from, Val, Acc) ->
				  maps:put(from, naddress:encode(Val), Acc);
			      (to, Val, Acc) ->
				  maps:put(to, naddress:encode(Val), Acc);
			      (Key, Val, Acc) -> maps:put(Key, Val, Acc)
			  end,
			  #{}, tx:unpack(NewTx))},
	       State#{seq => Seq + 1}};
	  Any -> {reply, {error, Any}, State}
	end
    catch
      Ec:Ee ->
	  S = hd(erlang:get_stacktrace()),
	  wf:info("Error ~p:~p at ~p", [Ec, Ee, S]),
	  {reply, {error, {Ec, Ee}}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(refetch, #{name := Name} = State) ->
    case config:faucet_settings(Name) of
      #{address := MyAddr, key := MyKey, url := URL} ->
	  try TA = binary_to_list(naddress:encode(MyAddr)),
	      error_logger:error_msg("C0 ~p", [TA]),
	      {ok, {{_HTTP11, 200, _OK}, _Headers, Body}} =
		  httpc:request(get, {URL ++ "/api/address/" ++ TA, []},
				[], [{body_format, binary}]),
	      #{<<"info">> := #{<<"seq">> := Seq}} = jsx:decode(Body,
								[return_maps]),
	      {noreply,
	       State#{seq => Seq, addr => MyAddr, key => MyKey,
		      url => URL}}
	  catch
	    _Ec:_Ee ->
		S = hd(erlang:get_stacktrace()),
		wf:info("MyWallet fetch error ~p:~p at ~p",
			[_Ec, _Ee, S]),
		erlang:send_after(10000, self(), refetch),
		{noreply, State}
	  end;
      _ ->
	  wf:info("MyWallet ~s Config parse error", [Name]),
	  erlang:send_after(60000, self(), refetch),
	  {noreply, State}
    end;
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

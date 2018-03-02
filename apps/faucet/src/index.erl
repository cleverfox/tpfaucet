-module(index).

-include_lib("nitro/include/nitro.hrl").

-include_lib("n2o/include/wf.hrl").

-export([event/1, main/0]).

main() ->
    #dtl{file = "index", app = faucet,
	 bindings =
	     [{body,
	       [
                #span{body = "Chain"},
                #dropdown{id=source,
                          options=
                          [ #option{ label=Descr, value=Name } || 
                           {Name,Descr} <- config:wallets_info()
                          ]
                          },
                #br{},
                #span{body = "Address"},
		#textbox{id = address, autofocus = true,
			 value =
			     naddress:encode(<<128, 1, 64, 0, 0, 0, 0, 5>>)},
		#br{}, #span{body = "Amount"},
		#textbox{id = amount, value = 10}, #br{},
		#button{id = giveButton, body = "Give me please",
			postback = give, source = [address, amount, source]},
		#br{}, #span{id = msg}]}]}.

event(give) ->
    wf:info("G ~p",[wf:q(source)]),
    Addr = try DA = naddress:decode(wf:q(address)),
	       wf:wire(#jq{target = address, property = style,
			   right = "background:white;"}),
	       DA
	   catch
	     _:_ ->
		 wf:wire(#jq{target = address, property = style,
			     right = "background:red;"}),
		 wf:wire(#jq{target = address,
			     method = [focus, select]}),
		 undefined
	   end,
    Amount = try A = binary_to_integer(wf:q(amount)),
		 if A > 100 ->
			wf:wire(#jq{target = amount, property = value,
				    right = "100"}),
			throw(over);
		    A < 1 ->
			wf:wire(#jq{target = amount, property = value,
				    right = "1"}),
			throw(under);
		    true ->
			wf:wire(#jq{target = amount, property = style,
				    right = "background:white;"}),
			A
		 end
	     catch
	       _:_ ->
		   wf:wire(#jq{target = amount, property = style,
			       right = "background:red;"}),
		   wf:wire(#jq{target = amount, method = [focus, select]}),
		   undefined
	     end,
    if Addr == undefined orelse Amount == undefined -> ok;
       true ->
           Wallet=list_to_existing_atom("wallet_"++binary_to_list(wf:q(source))),
           wf:info("Wallet ~p",[Wallet]),
	   case catch gen_server:call(Wallet, {give, Addr, Amount}) of
	     {ok, TxID, Tx} ->
		 JSTx = binary_to_list(jsx:encode(Tx)),
		 wf:info("JSTx ~s", [JSTx]),
		 wf:update(msg,
			   #span{id = msg,
				 body =
				     [#span{id = txid, body = TxID}, #br{},
				      #pre{id = txtxt, body = JSTx}]});
	     {error, Text} ->
                   wf:info("Error ~p",[Text]),
		 wf:update(msg,
			   #span{id = msg,
                                 class= <<"error">>,
                                 body = io_lib:format("Error: ~p", [Text])})
	   end
    end,
    [];
event(E) -> wf:info("Event ~p", [E]), [].

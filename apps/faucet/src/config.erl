-module(config).

-compile(export_all).

log_level() -> info.

log_modules() ->
    [login, wf, n2o_async, n2o_stream, index].

wallets() -> case file:consult("wallets.config") of
                 {ok, W} ->
                     [ N || #{name:=N} <- W ];
                 Error ->
                     wf:error("wallets load error ~p",[Error]),
                     []
             end.

wallets_info() -> case file:consult("wallets.config") of
                 {ok, W} ->
                     [ {N,maps:get(descr,I,atom_to_list(N))} || #{name:=N}=I <- W ];
                 Error ->
                     wf:error("wallets load error ~p",[Error]),
                     []
             end.

faucet_settings(Name) ->
    case file:consult("wallets.config") of
        {ok, W} ->
            lists:foldl(
              fun(#{name:=N}=V,undefined) when N==Name ->
                      V;
                 (_,Found) -> Found
              end, undefined, W);
        Error ->
            wf:error("wallets load error ~p",[Error]),
            []
    end.



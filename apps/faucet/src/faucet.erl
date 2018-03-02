-module(faucet).

-behaviour(supervisor).

-behaviour(application).

-export([init/1, main/1, start/0, start/2, stop/0,
	 stop/1]).

main(A) -> mad:main(A).

start() -> application:ensure_all_started(faucet).

stop() -> application:stop(faucet).

start(_, _) ->
    supervisor:start_link({local, faucet}, faucet, []).

stop(_) -> ok.

init([]) ->
    Routes = [{'_',
	       [{"/static/[...]", n2o_static,
		 {dir, "apps/faucet/priv/static", mime()}},
		{"/n2o/[...]", n2o_static,
		 {dir, "deps/n2o/priv", mime()}},
		{"/multipart/[...]", n2o_multipart, []},
		{"/rest/:resource", rest_cowboy, []},
		{"/rest/:resource/:id", rest_cowboy, []},
		{"/ws/[...]", n2o_stream, []}, {'_', n2o_cowboy, []}]}],
    Opts = [{env,
	     [{dispatch, cowboy_router:compile(Routes)}]}],
    Port = [{port, wf:config(n2o, port, 8000)}],
    Wallets = [{list_to_atom("wallet_" ++ atom_to_list(N)),
		{mywallet, start_link, [N]}, permanent, 5000, worker,
		[]}
	       || N <- config:wallets()],
    io:format("~p~n", [Wallets]),
    {ok,
     {{one_for_one, 5, 10},
      [ranch:child_spec(http, 100, ranch_tcp, Port,
			cowboy_protocol, Opts)
       | Wallets]}}.

mime() -> [{mimetypes, cow_mimetypes, all}].

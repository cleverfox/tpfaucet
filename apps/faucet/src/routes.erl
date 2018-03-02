-module(routes).

-include_lib("nitro/include/nitro.hrl").

-include_lib("n2o/include/wf.hrl").

-export([finish/2, init/2]).

finish(State, Ctx) -> {ok, State, Ctx}.

init(State, Ctx) ->
    Path = wf:path(Ctx#cx.req),
    wf:info(routes, "Route: ~p~n", [Path]),
    {ok, State,
     Ctx#cx{path = Path, module = route_prefix(Path)}}.

route_prefix(<<"/ws/", P/binary>>) -> route(P);
route_prefix(<<"/", P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>) -> index;
route(_) -> index.

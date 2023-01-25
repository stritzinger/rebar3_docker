-module(rebar3_docker).


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/1]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, AllDeps} = application:get_key(?MODULE, applications),
    Deps = AllDeps -- [kernel, stdlib],
    [{ok, _} = application:ensure_all_started(A) || A <- Deps],
    lists:foldl(fun(Mod, {ok, S}) -> Mod:init(S) end, {ok, State}, [
        rebar3_docker_build
    ]).

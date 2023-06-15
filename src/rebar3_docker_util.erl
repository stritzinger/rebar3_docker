-module(rebar3_docker_util).


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% API functions
-export([config/1]).
-export([container_dir/1]).


%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DEFAULT_ERLANG_VERSION, <<"25.3.2.2">>).
-define(CONFIG_KEYS, [
        {tag, binary},
        {erlang_version, binary},
        {builder_image, binary},
        {appname, binary},
        {build_packages, [list, binary]},
        {git_url_rewrites, [list, [tuple, [binary, binary]]]},
        {runtime_packages, [list, binary]},
        {ports, [list, [tuple, [integer, {tcp, udp}]]]},
        {env, [list, [tuple, [binary, any]]]}
]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

config(RState) ->
    UserCfg = maps:merge(default_config(RState), docker_config(RState)),
    define_builder_image(UserCfg).

container_dir(RState) ->
    filename:join([rebar_state:dir(RState),
                   "_build",
                   atom_to_list(hd(rebar_state:current_profiles(RState))),
                   "container"]).

%%% INTERNAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

define_builder_image(#{builder_image := _} = Cfg) -> Cfg;
define_builder_image(#{erlang_version := ErlV} = Cfg) ->
    Cfg#{builder_image => "erlang:" ++ ErlV ++ "-alpine" }.

release_config(RState) ->
    Config =  rebar_state:get(RState, relx, []),
    case lists:keyfind(release, 1, Config) of
        {release, {Name, _Ver}, _Apps} ->
            convert_docker_config(release_name, binary, Name);
        false -> undefined
    end.

docker_config(RState) ->
    Config = rebar_state:get(RState, docker, []),
    parse_docker_config(?CONFIG_KEYS, Config, #{}).

parse_docker_config([], _Config, Acc) -> Acc;
parse_docker_config([{Name, Type} | Rest], Config, Acc) ->
    case lists:keyfind(Name, 1, Config) of
        {Name, RawValue} ->
            Acc2 = Acc#{Name => convert_docker_config(Name, Type, RawValue)},
            parse_docker_config(Rest, Config, Acc2);
        false ->
            parse_docker_config(Rest, Config, Acc)
    end.

convert_docker_config(_Name, any, V) -> V;
convert_docker_config(_Name, binary, V) when is_binary(V) -> V;
convert_docker_config(_Name, integer, V) when is_integer(V) -> V;
convert_docker_config(_Name, binary, V) when is_list(V) -> list_to_binary(V);
convert_docker_config(_Name, binary, V) when is_integer(V) -> integer_to_binary(V);
convert_docker_config(_Name, binary, V) when is_atom(V) -> atom_to_binary(V);
convert_docker_config(Name, integer, V) when is_binary(V) ->
    try binary_to_integer(V)
    catch _:badarg -> erlang:error({bad_config, Name})
    end;
convert_docker_config(Name, integer, V) when is_list(V) ->
    try list_to_integer(V)
    catch _:badarg -> erlang:error({bad_config, Name})
    end;
convert_docker_config(Name, [list, Type], V) when is_list(V) ->
    [convert_docker_config(Name, Type, I) || I <- V];
convert_docker_config(Name, [tuple, Types], V) when is_tuple(V) ->
    list_to_tuple([convert_docker_config(Name, T, I) ||
                   {T, I} <- lists:zip(Types, tuple_to_list(V))]);
convert_docker_config(Name, Enum, V) when is_tuple(Enum) ->
    case lists:member(V, tuple_to_list(Enum)) of
        true -> V;
        false -> erlang:error({bad_config, Name})
    end;
convert_docker_config(Name, _T, _V) ->
    erlang:error({bad_config, Name}).

default_config(RState) ->
    AppName = release_config(RState),
    #{
        tag => iolist_to_binary(io_lib:format("local/~s", [AppName])),
        appname => AppName,
        env => [],
        erlang_version => ?DEFAULT_ERLANG_VERSION,
        git_url_rewrites => [],
        ports => [],
        build_packages => [],
        runtime_packages => []
    }.
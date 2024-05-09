-module(rebar3_docker_build).


%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/1]).
-export([do/1]).
-export([format_error/1]).


%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(PROVIDER, docker).
-define(DEPS, [app_discovery]).


%%% API FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(RState) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 docker build"}, % How to use the plugin
            {opts, [                      % list of options understood by the plugin
            ]},
            {short_desc, "A rebar plugin to generate docker containers"},
            {desc, "A rebar plugin to generate docker containers"}
    ]),
    {ok, rebar_state:add_provider(RState, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(RState) ->
    case rebar_prv_lock:do(RState) of
        {ok, RState2} ->
            Config = rebar3_docker_util:config(RState2),
            Params = template_parameters(RState2, Config),
            TemplateFile = filename:join([code:priv_dir(rebar3_docker),
                                          "templates", "Dockerfile"]),
            Template = bbmustache:parse_file(TemplateFile),
            Data = bbmustache:compile(Template, Params,
                [{key_type, atom}, {escape_fun, fun(X) -> X end}]),
            ContainerDir = rebar3_docker_util:container_dir(RState2),
            DataFilename = filename:join(ContainerDir, "Dockerfile"),
            filelib:ensure_dir(DataFilename),
            file:write_file(DataFilename, Data),
            Args = ["build", ".", "-f", DataFilename, "--progress=plain",
                    "--tag", binary_to_list(maps:get(tag, Config))],
            Args2 = case maps:get(platform, Config) of
                        [] -> Args;
                        Platforms -> Args ++ format_platform_option(Platforms)
                    end,
            Args3 = case os:getenv("SSH_AUTH_SOCK") of
                false -> Args2;
                Value -> Args2 ++ ["--ssh=default=" ++ Value]
            end,
            CmdOpts = [use_stdout, abort_on_error, {cd, rebar_state:dir(RState2)}],
            Command = lists:flatten(lists:join(" ", ["docker" | Args3])),
            rebar_utils:sh(Command, CmdOpts),
            {ok, RState2};
        Error ->
            Error
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

template_parameters(RState, Config) ->
    Config#{
        profile => hd(lists:reverse(rebar_state:current_profiles(RState))),
        env => [#{name => N, value => escape_string(V)}
                || {N, V} <- maps:get(env, Config)],
        git_url_rewrites => [#{from => F, to => T}
                             || {F, T} <- maps:get(git_url_rewrites, Config)],
        ports => [#{port => Port, protocol => Proto}
                  || {Port, Proto} <- maps:get(ports, Config)],
        build_packages => [#{name => N}
                           || N <- maps:get(build_packages, Config)],
        runtime_packages => [#{name => N}
                            || N <- maps:get(runtime_packages, Config)]
    }.

escape_string(Str) when is_binary(Str) ->
    iolist_to_binary(io_lib:format("\"~s\"",
        [rebar_utils:escape_double_quotes(Str)]));
escape_string(Str) when is_list(Str) ->
    iolist_to_binary(io_lib:format("\"~s\"",
        [rebar_utils:escape_double_quotes(list_to_binary(Str))]));
escape_string(Other) ->
    Other.

format_platform_option(Platforms) ->
    ["--platform",
     lists:join(",", lists:map(fun(Platform) ->
                                       binary_to_list(Platform)
                               end, Platforms))].

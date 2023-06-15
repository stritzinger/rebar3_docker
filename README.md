rebar3_docker
=====

A simple rebar plugin to build docker containers out of an Erlang release.


Build
-----

    $ rebar3 compile


Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_docker, {git, "https://github.com/stritzinger/rebar3_docker.git", {branch, "main"}}}
    ]}.

Add docker the configuration as needed to your rebar config:

    {docker, [
        % The tag to be used to publish the docker image.
        % If not specified, uses "locale/release_name".
        {tag, "local/somename"},
        % The version of the erlang docker image.
        % If not specified, uses "25.3.2.2"
        {erlang_version, "25.3.2.2"},
        % If you need to further customize the building environment,
        % this option overrides the official erlang image with anything you want.
        % in this case the erlang_version option is ignored 
        % Must still be an alpine based image and contain OTP + Rebar3!
        % {builder_image, "your-repo/your-image:tag"},
        % The name of the application release.
        % If not specified use the first release name found in the relx config.
        {appname, "appname"},
        % The extra packages to install in the building docker layer.
        {build_packages, [            % 
            make,
            gcc,
            "libc-dev",
            "libbsd-dev"
        ]},
        % The git url to be rewritten. Used to access private repository.
        {git_url_rewrites, [
            {"https://github.com/", "git@github.com:"}
        ]},
        % The extra runtime package to install in the final docker image.
        {runtime_packages, []},
        % The ports to be exposed by the docker image.
        {ports, [
            {8888, tcp}
        ]},
        % The docker image environment.
        {env, [
            {'COOKIE', "dummy"},
            {'LOGGER_LEVEL', debug}
        ]}
    ]}

Be sure to configure a relx release and then just call the plugin build command:

    $ rebar3 docker build

Example of a minimal working configuration for a release using port 1234,
and defining logging and cookie environment variables:

    {plugins, [
        {rebar3_docker, {git, "https://github.com/stritzinger/rebar3_docker.git",
                         {tag, "0.1.0"}}}
    ]}.
    {deps, []}.
    {docker, [
        {ports, [{1234, tcp}]},
        {env, [
            {'COOKIE', "dummy"},
            {'LOGGER_LEVEL', debug}
        ]}
    ]}.
    {relx, [
        {release, {myapp, "1.0.0"}, [myapp, sasl]},
        {include_src, false},
        {include_erts, true}
    ]}.


STARTING A CONTAINER
--------------------

When the image as been build you can create a container and start it with:

    docker create --name myapp local/myapp
    docker start myapp

Or run it interactively in console mode:

    docker run --rm -it local/myapp console

If your application export some ports, you will need to explicitly publish it
with the docker argument `-p 1234:1234`.


DEBUGGING
---------

When running the build command, the docker configuration is saved in:
    
    _build/[PROFILE]/container/Dockerfile


TODO
----

Add support for multiple profiles.

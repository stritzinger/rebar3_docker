rebar3_docker
=====

A simple rebar plugin to build Docker images from an Erlang release.


Build
-----

```sh
rebar3 compile
```


Use
---

Add the plugin to your `rebar.config`:

```erlang
{plugins, [
    rebar3_docker
]}.
```

Then configure the Docker build:

```erlang
{docker, [
    % The tag used when publishing the Docker image.
    % If not specified, defaults to "local/<release_name>".
    {tag, "local/somename"},

    % The official Erlang builder image tag, expressed as the OTP version.
    % Defaults to "28.4.0.0", which becomes "erlang:28.4.0.0-alpine".
    {erlang_version, "28.4.0.0"},

    % Override the entire build image when you need a custom Alpine-based
    % image that already contains OTP + Rebar3.
    % If this is set, erlang_version is ignored.
    % {builder_image, "your-repo/your-image:tag"},

    % The name of the application release.
    % If not specified, uses the first release name found in relx config.
    {appname, "appname"},

    % Extra Alpine packages needed while compiling the release.
    {build_packages, [
        make,
        gcc,
        "libc-dev",
        "libbsd-dev"
    ]},

    % Git URL rewrites used while fetching dependencies.
    {git_url_rewrites, [
        {"https://github.com/", "git@github.com:"}
    ]},

    % Extra Alpine packages installed in the runtime image.
    {runtime_packages, []},

    % Ports exposed by the runtime image.
    {ports, [
        {8888, tcp}
    ]},

    % Default environment variables for the runtime image.
    {env, [
        {'COOKIE', "dummy"},
        {'LOGGER_LEVEL', debug}
    ]},

    % Target platforms passed to docker build.
    % You can specify multiple values if your Docker setup supports it.
    {platform, ["linux/arm64"]}
]}.
```

Be sure to configure a `relx` release and then run:

```sh
rebar3 docker build
```


OTP Versions
------------

This plugin builds the release in an Erlang image and runs it in a final
Alpine image.

The intended setup is to bundle ERTS into the release:

```erlang
{relx, [
    {release, {myapp, "1.0.0"}, [myapp, sasl]},
    {include_src, false},
    {include_erts, true}
]}.
```

With `{include_erts, true}`, the release carries the OTP runtime built in the
builder stage. That means changing `erlang_version` also changes the OTP
version used when the release starts inside the final container.

This plugin assumes that model. If you do not bundle ERTS, the final image
would need its own OTP installation, and that is intentionally not handled
here.


Upgrading OTP
-------------

To move from the old default `25.3.2.2` line to OTP 27 or 28, choose one of
these approaches for the builder image.

Use the official Erlang image tag:

```erlang
{docker, [
    {erlang_version, "28.4.0.0"}
]}.
```

That produces a builder image of `erlang:28.4.0.0-alpine`.

Use your own build image:

```erlang
{docker, [
    {builder_image, "ghcr.io/your-org/erlang-build:otp-28.4.0.0"}
]}.
```

Notes:

- `erlang_version` accepts any official Erlang tag component, so `27`,
  `27.3`, `28`, and `28.4.0.0` all work. Pinned versions are more
  reproducible than floating major tags.
- `builder_image` must remain Alpine-based because the generated Dockerfile
  installs packages with `apk`.
- Keep `{include_erts, true}` so the runtime OTP inside the container matches
  the OTP used to build the release.


Example
-------

Minimal working configuration for a release exposing port `1234` and running
on OTP 28:

```erlang
{plugins, [
    rebar3_docker
]}.

{deps, []}.

{docker, [
    {erlang_version, "28.4.0.0"},
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
```


Starting A Container
--------------------

When the image has been built you can create a container and start it with:

```sh
docker create --name myapp local/myapp
docker start myapp
```

Or run it interactively in console mode:

```sh
docker run --rm -it local/myapp console
```

If your application exposes ports, publish them explicitly with `-p`, for
example `-p 1234:1234`.


Debugging
---------

The generated Dockerfile is written to:

```text
_build/[PROFILE]/container/Dockerfile
```

That file is the easiest way to confirm which images are actually being used,
for example:

- `FROM erlang:28.4.0.0-alpine as builder`
- `FROM alpine as runner`


TODO
----

Add support for multiple profiles.

# syntax = docker/dockerfile:experimental

#--- Builder -------------------------------------------------------------------

ARG profile={{profile}}
ARG erlang_version={{erlang_version}}
FROM erlang:$erlang_version-alpine as builder

WORKDIR /app/src
ENV REBAR_BASE_DIR /app/_build

# Install git for fetching non-hex depenencies.
# Add any other Alpine libraries needed to compile the project here.
# See https://wiki.alpinelinux.org/wiki/Local_APK_cache for details
# on the local cache and need for the symlink
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update \
{{#build_packages}}
        {{name}} \
{{/build_packages}}
        git

# build and cache dependencies as their own layer
COPY rebar.config rebar.lock ./

RUN apk add --no-cache openssh-client git && \
{{#git_url_rewrites}}
    git config --global url."{{to}}".insteadOf "{{from}}" && \
{{/git_url_rewrites}}
    mkdir -p -m 0600 ~/.ssh && \
    ssh-keyscan github.com >> ~/.ssh/known_hosts

RUN --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 --mount=type=ssh,required=true \
    rebar3 compile

#--- Profile Builder -----------------------------------------------------------

FROM builder as builder-profile

ARG profile
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as $profile compile

#--- Releaser ------------------------------------------------------------------

FROM builder-profile as releaser

# create the directory to unpack the release to
RUN mkdir -p /opt/rel

# tar for unpacking the target system
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    apk add --update tar

ARG profile
RUN --mount=target=. \
    --mount=id=hex-cache,type=cache,sharing=locked,target=/root/.cache/rebar3 \
    rebar3 as $profile release && \
    rebar3 as $profile tar && \
    tar -zxvf $REBAR_BASE_DIR/$profile/rel/*/*.tar.gz -C /opt/rel

#--- Runner --------------------------------------------------------------------

FROM alpine as runner

WORKDIR /opt/{{appname}}/

ENV \
    # application specific env variables to act as defaults
{{#env}}
    {{name}}={{value}} \
{{/env}}
    # write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp

# openssl needed by the crypto app
RUN --mount=type=cache,id=apk,sharing=locked,target=/var/cache/apk \
    ln -s /var/cache/apk /etc/apk/cache && \
    apk add --update \
{{#runtime_packages}}
        {{name}} \
{{/runtime_packages}}
        libstdc++ \
        openssl \
        ncurses

COPY --from=releaser /opt/rel .

{{#ports}}
EXPOSE {{port}}/{{protocol}}
{{/ports}}

ENTRYPOINT ["/opt/{{appname}}/bin/{{appname}}"]
CMD ["foreground"]

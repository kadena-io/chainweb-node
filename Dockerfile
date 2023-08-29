# syntax = docker/dockerfile:1.4

# INSTRUCTIONS
#
# Build local context (chainweb-node repo):
#
# ```sh
# docker buildx build .
# ```
#
# Build remote context from github:
#
# ```
# docker buildx build --build-arg BUILDKIT_CONTEXT_KEEP_GIT_DIR=1 https://github.com/kadena-io/chainweb-node.git#master
# ```
#
# Setting `BUILDKIT_CONTEXT_KEEP_GIT_DIR=1` is optional but enables some
# additional sanity checks.
#
# Skipping tests:
#
# ```sh
# docker buildx build --target=chainweb-node .
# ```
#
# Usefull target values are:
#
# - chainweb-node, image with only chainweb-node
# - chainweb-applications, image with all executables from the repository
# - chainweb-node-tested (default), just chainweb-node, runs tests during build
#
# Troubleshooting:
#
# If the build is failing because of unavailable disk space, try pruning the
# build cache with
#
# ```sh
# docker buildx prune --all
# ```

# ############################################################################ #
# Parameters
# ############################################################################ #

# changing the ubuntu version will most likely break the build. In order to
# support this we would have to define dedicated runtime images and build
# images.

ARG UBUNTU_VERSION=20.04
ARG GHC_VERSION=9.6.2

# ############################################################################ #
# Chainweb Application Runtime Image
# ############################################################################ #

FROM ubuntu:${UBUNTU_VERSION} AS chainweb-runtime
ARG GHC_VERSION
ARG UBUNTU_VERSION
ARG TARGETPLATFORM
RUN apt-get update -y \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -yqq \
        --no-install-recommends \
        ca-certificates \
        libssl1.1 \
        zlib1g \
        locales \
        libffi7 \
        libgmp10 \
        libncurses5 \
        libtinfo5 \
        libsnappy1v5 \
        zlib1g \
        liblz4-1 \
        libbz2-1.0 \
        libgflags2.2 \
    && if [ "${TARGETPLATFORM}" = "linux/arm64" ] ; then \
        apt-get install -yqq \
            --no-install-recommends \
            llvm-12 \
            libnuma1 ; \
       fi \
    && rm -rf /var/lib/apt/lists/* \
    && locale-gen en_US.UTF-8 \
    && update-locale LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8
WORKDIR /chainweb
LABEL com.chainweb.docker.image.compiler="ghc-${GHC_VERSION}"
LABEL com.chainweb.docker.image.os="ubuntu-${UBUNTU_VERSION}"

# ############################################################################ #
# Chainweb Build Image
# ############################################################################ #

FROM chainweb-runtime AS chainweb-build
ARG GHC_VERSION
ARG TARGETPLATFORM
RUN apt-get update -y \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -yqq \
        --no-install-recommends \
        libbz2-dev \
        libffi-dev \
        libgflags-dev \
        libgmp-dev \
        liblz4-dev \
        libncurses-dev \
        libsnappy-dev \
        libssl-dev \
        libzstd-dev \
        zlib1g-dev \
        binutils \
        build-essential \
        pkg-config \
        git \
        curl \
    && if [ ${TARGETPLATFORM} = "linux/arm64" ] ; then \
        echo plat: ${TARGETPLATFORM} && \
        apt-get install -yqq \
            --no-install-recommends \
            llvm-12 \
            libnuma-dev ; \
       fi \
    && rm -rf /var/lib/apt/lists/*
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_MINIMAL=1
ENV BOOTSTRAP_HASKELL_NO_UPGRADE=1
RUN --mount=type=cache,target=/root/.ghcup/cache,id=${TARGETPLATFORM} \
    curl -sSf https://get-ghcup.haskell.org | sh
ENV PATH=/root/.local/bin:$PATH
ENV PATH=/root/.ghcup/bin:$PATH
ENV PATH=/root/.cabal/bin:$PATH
ENV LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu/:$LD_LIBRARY_PATH
RUN --mount=type=cache,target=/root/.ghcup/cache,id=${TARGETPLATFORM} \
    ghcup --cache install cabal latest \
    && ghcup set cabal latest
RUN --mount=type=cache,target=/root/.ghcup/cache,id=${TARGETPLATFORM} \
    ghcup --cache install ghc $GHC_VERSION \
    && ghcup set ghc $GHC_VERSION
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    cabal update

# ############################################################################ #
# Builds
# ############################################################################ #

# ############################################################################ #
# Setup Context

FROM chainweb-build as chainweb-build-ctx
ARG TARGETPLATFORM
COPY . .
ENV GIT_DISCOVERY_ACROSS_FILESYSTEM=1
RUN mkdir -p /tools
COPY <<EOF /tools/check-git-clean.sh
#!/bin/sh
if [ -d ".git" ] && ! [ -f "/tools/wip" ] && ! git diff --exit-code; then \
    echo "Git working tree is not clean. The build changed some file that is checked into git." 1>&2 ; \
    exit 1 ; \
fi
EOF
RUN sh /tools/check-git-clean.sh || touch /tools/wip

# ############################################################################ #
# Build Dependencies

FROM chainweb-build-ctx as chainweb-build-dependencies
ARG TARGETPLATFORM
# RUN git clone --filter=tree:0 https://github.com/kadena-io/chainweb-node
# WORKDIR /chainweb/chainweb-node
# RUN git checkout $GIT_REV
COPY . .
ENV GIT_DISCOVERY_ACROSS_FILESYSTEM=1
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    [ -f cabal.project.freeze ] || cabal --enable-tests --enable-benchmarks freeze
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks --only-download
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks --only-dependencies

# ############################################################################ #
# Build Chainweb Library

FROM chainweb-build-dependencies AS chainweb-build-lib
ARG TARGETPLATFORM
ENV GIT_DISCOVERY_ACROSS_FILESYSTEM=1
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks chainweb:lib:chainweb
RUN sh /tools/check-git-clean.sh

# ############################################################################ #
# Build Chainweb Tests

FROM chainweb-build-lib AS chainweb-build-tests
ARG TARGETPLATFORM
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks chainweb:test:chainweb-tests
RUN sh /tools/check-git-clean.sh
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    mkdir -p artifacts \
    && cp $(cabal list-bin --enable-tests --enable-benchmarks chainweb:test:chainweb-tests) artifacts/

# ############################################################################ #
# Build cwtool and run ea

FROM chainweb-build-lib AS chainweb-build-cwtool
ARG TARGETPLATFORM
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks chainweb:exe:cwtool
RUN sh /tools/check-git-clean.sh
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal run --enable-tests --enable-benchmarks chainweb:exe:cwtool -- ea \
RUN sh /tools/check-git-clean.sh || \
    { echo "Inconsistent genesis headers detected. Did you forget to run ea?" 1>&2 ; exit 1 ; }
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    mkdir -p artifacts \
    && cp $(cabal list-bin --enable-tests --enable-benchmarks chainweb:exe:cwtool) artifacts/

# ############################################################################ #
# Build benchmarks

FROM chainweb-build-lib AS chainweb-build-bench
ARG TARGETPLATFORM
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks chainweb:bench:bench
RUN sh /tools/check-git-clean.sh
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    mkdir -p artifacts \
    && cp $(cabal list-bin --enable-tests --enable-benchmarks chainweb:bench:bench) artifacts/

# ############################################################################ #
# Build Chainweb Node Application

FROM chainweb-build-lib AS chainweb-build-node
ARG TARGETPLATFORM
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    cabal build --enable-tests --enable-benchmarks chainweb:exe:chainweb-node
RUN sh /tools/check-git-clean.sh
RUN --mount=type=cache,target=/root/.cabal,id=${TARGETPLATFORM} \
    --mount=type=cache,target=./dist-newstyle,id=chainweb-${TARGETPLATFORM},sharing=locked \
    mkdir -p artifacts \
    && cp $(cabal list-bin --enable-tests --enable-benchmarks chainweb:exe:chainweb-node) artifacts/

# ############################################################################ #
# Run Tests and Benchmarks
# ############################################################################ #

# ############################################################################ #
# Run Chainweb Tests

FROM chainweb-runtime AS chainweb-run-tests
COPY --from=chainweb-build-tests /chainweb/artifacts/chainweb-tests .
COPY --from=chainweb-build-tests /chainweb/test/pact test/pact
COPY --from=chainweb-build-tests /chainweb/pact pact
RUN ulimit -n 10000 \
    && ./chainweb-tests --hide-successes --results-json test-results.json

# ############################################################################ #
# Run slow tests

FROM chainweb-runtime AS chainweb-run-slowtests
COPY --from=chainweb-build-cwtool /chainweb/artifacts/cwtool .
RUN ulimit -n 10000 \
    && ./cwtool slow-tests

# ############################################################################ #
# Run benchmarks

FROM chainweb-runtime AS chainweb-run-bench
COPY --from=chainweb-build-bench /chainweb/artifacts/bench .
RUN ulimit -n 10000 \
    && ./bench

# ############################################################################ #
# Applications
# ############################################################################ #

# ############################################################################ #
# Chainweb-node Application

FROM chainweb-runtime AS chainweb-node
COPY --from=chainweb-build-node /chainweb/artifacts/chainweb-node .
COPY --from=chainweb-build-node /chainweb/LICENSE .
COPY --from=chainweb-build-node /chainweb/README.md .
COPY --from=chainweb-build-node /chainweb/CHANGELOG.md .
COPY --from=chainweb-build-node /chainweb/chainweb.cabal .
COPY --from=chainweb-build-node /chainweb/cabal.project .
COPY --from=chainweb-build-node /chainweb/cabal.project.freeze .
STOPSIGNAL SIGTERM
HEALTHCHECK CMD \
    [ $(ulimit -Sn) -gt 65535 ] \
    && exec 3<>/dev/tcp/localhost/1848 \
    && printf "GET /health-check HTTP/1.1\r\nhost: http://localhost:1848\r\nConnection: close\r\n\r\n" >&3 \
    && grep -q "200 OK" <&3 \
    || exit 1
ENTRYPOINT ["/chainweb/chainweb-node"]

# ############################################################################ #
# All binaries (for testing and debugging)

FROM chainweb-node AS chainweb-applications
COPY --from=chainweb-build-bench /chainweb/artifacts/bench .
COPY --from=chainweb-build-cwtool /chainweb/artifacts/cwtool .
COPY --from=chainweb-build-tests /chainweb/artifacts/chainweb-tests .
COPY --from=chainweb-build-tests /chainweb/test/pact test/pact
COPY --from=chainweb-build-tests /chainweb/pact pact

# ############################################################################ #
# Tested Chainweb-node Application

FROM chainweb-node AS chainweb-node-tested
# Phony dependencies on tests
COPY --from=chainweb-build-cwtool /etc/hostname /tmp/run-ea
COPY --from=chainweb-run-tests /etc/hostname /tmp/run-tests
COPY --from=chainweb-run-slowtests /etc/hostname /tmp/run-slowtests
COPY --from=chainweb-run-bench /etc/hostname /tmp/run-bench
RUN rm -f /tmp/run-tests /tmp/run-ea /tmp/run-slowtests /tmp/run-bench

# ############################################################################ #
# (Optional) Initialize and Validate Database
# ############################################################################ #

# FROM CHAINWEB_BUILD AS CHAINWEB_INITIALIZE_DB
#
# TODO
# - Create image that only rocksdb database to volume, so that it can be
#   done during the build?
# - Just start the node on it?


#!/bin/bash

# ############################################################################ #
# Usage

# The build uses the ghc binary in the PATH. It assumes that all dependencies
# are available in the user package database.

function usage () {
    echo "USAGE: run-nodes LOG_DIRECTORY [NUMBER_OF_NODES]"
}

# FIXME: port allocation doesn't check if ports are already in use
# this is better fixed by supporting passing port 0 to chainweb-node.

# ############################################################################ #
# Configuration

[[ -f 'chainweb.cabal' ]] || { echo "script not called from chainweb project directory" 1>&2 ; exit -1 ; }
[[ "$#" -ge 1 ]] || { echo "missing madatory argument" 1>&2 ; usage ; exit -1 ; }

LOG_DIR=$1 && shift

# Number of nodes
N=${1:-10}

rm -f $LOG_DIR/cuts.node*.json

# gnu-sed binary (on linux use 'sed' on mac usually 'gsed')
SED=gsed

BUILD="cabal new-build exe:chainweb-node"

RUN="cabal new-run exe:chainweb-node --"

# ############################################################################ #
# some sanity checks

[[ "$N" -ge 1 ]] || { echo "number of nodes \"$N\" to small" 1>&2 ; exit -1 ; }
[[ "$N" -le 100 ]] || { echo "number of nodes \"$N\" to big" 1>&2 ; exit -1 ; }
[[ -d "$LOG_DIR" ]] || { echo "log directory \"$LOG_DIR\" doesn't exist" 1>&2 ; usage 1>&2 ; exit -1 ; }
[[ -x `which ghc` ]] || { echo "no ghc executable" 1>&2 ; exit -1 ; }
[[ -x `which $SED` ]] || { echo "no $SED executable" 1>&2 ; exit -1 ; }

# ############################################################################ #
# functions

function jsonLogs () {
    for i in $LOG_DIR/cuts.node*.log ; do
        ${SED} -e 's/^/,/;1s/^./[/;$a]' "$i" > "${i%.log}.json"
    done
}

function build () {
    $BUILD
}

function onExit () {
    kill $(jobs -p)
    jsonLogs
}

trap onExit EXIT

# ############################################################################ #
# Build

echo "build chainweb-node executable"
build || { echo "build of chainweb-node failed" 1>&2 ; exit -1 ; }

# ############################################################################ #
# run nodes

echo "starting $N chainweb nodes"

$RUN \
    --peer-id=525ff65f-9240-4ada-9c36-fe7da982b4b4 \
    --cuts-logger-backend-handle="file:$LOG_DIR/cuts.node0.log" \
    --logger-backend-handle="file:$LOG_DIR/node0.log" \
    --mean-block-time=$((10 * N)) \
    --log-level=info &
echo "started node 0"

for ((i=1; i<N; i++)) ; do
    sleep 0.2
    $RUN \
        --node-id=${i} \
        --log-level=info \
        --port=0 \
        --hostname=127.0.0.1 \
        --cuts-logger-backend-handle="file:$LOG_DIR/cuts.node${i}.log" \
        --logger-backend-handle="file:$LOG_DIR/node${i}.log" \
        --mean-block-time=$((10 * N)) &
    echo "started node $i"
done

# ############################################################################ #
# Periodically publish to notebook

while true ; do
    jsonLogs || { echo "failed to publish cut logs" 1>&2 ; exit -1 ; }
    echo "published cut logs"
    sleep 10
done


#!/bin/bash

# ############################################################################ #
# Usage

function usage () {
    echo "USAGE:"
    echo
    echo "    run-nodes CHAINWEB_NODE_EXE NUMBER_OF_NODES [LOG_DIRECTORY|es:ELASTICSEARCH_HOST:PORT]"
    echo
    echo "If third argument starts with 'es:' it is used for logging to Elasticsearch."
    echo "NUMBER_OF_NODES must be between 1 and 100."
    echo "Stop nodes with Ctrl-C"
}

# ############################################################################ #
# Configuration

function err () {
    echo "Error:" 1>&2
    echo -n "    " 1>&2
    echo "$@" 1>&2
    echo 1>&2
    usage 1>&2
}

function transaction-index-flags () {
    if (( ! ${TRANSACTION_INDEX:-0} )); then
        echo "disabling tx index" >/dev/stderr
        echo "--disable-transaction-index"
    fi
}

LOGLEVEL=${LOGLEVEL:-info}
[ "$#" -ge 2 ] || { err "Missing arguments" ; exit -1 ; }

# Chainweb-node application
RUN=$1 && shift

# Number of nodes
N=$1 && shift

LOG_DIR=$1 && shift

# Disable Pact until pact integration passes all tests
export CHAINWEB_DISABLE_PACT=${CHAINWEB_DISABLE_PACT:-1}
[ "$CHAINWEB_DISABLE_PACT" -ne "0" ] && echo "pact is disabled"

# ############################################################################ #
# some sanity checks

# Check chainweb-node application
[ -x "$RUN" ] || { err "chainweb-node \"$RUN\" can't be exectuted" ; exit -1 ; }

# check number of nodes
[ "$N" -ge 1 ] || { err "number of nodes \"$N\" to small" ; exit -1 ; }
[ "$N" -le 100 ] || { err "number of nodes \"$N\" to big" ; exit -1 ; }

# check logdir

ES=$([[ $LOG_DIR =~ ^es:.*:[0-9]+$ ]] && echo 1)

[ -z "$LOG_DIR" -o -n "$ES" -o -d "$LOG_DIR" ] || { err "log directory \"$LOG_DIR\" doesn't exist" ; exit -1 ; }

# ############################################################################ #
# Kill all nodes on exit

function onExit () {
    kill $(jobs -p)
}

trap onExit EXIT

# ############################################################################ #
# Run Node

function run-node () {
    local NID=$1
    local CONFIG_FILE_ARG=$2

    if [[ -n "$LOG_DIR" ]] ; then

        # Decide handles for logs (easticsearch or file)
        local APP_LOG=""
        local TELEMETRY_LOG=""
        if [[ -n "$ES" ]] ; then
            APP_LOG="$LOG_DIR"
            TELEMETRY_LOG="$LOG_DIR"
        else
            TELEMETRY_LOG="file:$LOG_DIR/telemetry.node$NID.log"
            APP_LOG="file:$LOG_DIR/node$NID.log"
        fi

        # Run with LOG_DIR
        $RUN \
            --hostname=127.0.0.1 \
            --node-id=$NID \
            --test-miners=$N \
            --chainweb-version=testWithTime \
            --interface=127.0.0.1 \
            --log-level=$LOGLEVEL \
            --telemetry-log-handle="$TELEMETRY_LOG" \
            --log-handle="$APP_LOG" \
            $(transaction-index-flags) \
            $CONFIG_FILE_ARG \
            +RTS -T &

    else

        # Run without LOG_DIR
        $RUN \
            --hostname=127.0.0.1 \
            --node-id=$NID \
            --test-miners=$N \
            --chainweb-version=testWithTime \
            --interface=127.0.0.1 \
            --log-level=$LOGLEVEL \
            $(transaction-index-flags) \
            $PORT_ARG \
            $CONFIG_FILE_ARG \
            +RTS -T &
    fi
}

echo "starting $N chainweb nodes"

# Start P2P bootstrap node
#
# a bootstrap node is a node with a well defined peer-info (peer-id and
# hostaddress) that is know to all other nodes on startup. For the Test
# chainweb-node application the bootstrap node peer-info is compiled
# into the initial peer-database.

run-node 0 "--config-file=scripts/test-bootstrap-node.config"
echo "started bootstrap node 0"

# Start remaining nodes
#
# When no peer-id is configured a random peer-id is generated on startup.
# Omitting the port argument is the same as using --port=0, which means
# that a some free port is assigned to the node.

for ((i=1; i<N; i++)) ; do
    sleep 0.2
    run-node $i ""
    echo "started node $i"
done

wait

#!/bin/bash

# ############################################################################ #
# Usage

function usage () {
    echo "USAGE: run-nodes CHAINWEB_NODE_EXE NUMBER_OF_NODES [LOG_DIRECTORY]"
    echo "stop nodes with Ctrl-C"
}

# ############################################################################ #
# Configuration

LOGLEVEL=info

[ "$#" -ge 2 ] || { echo -e "Missing arguments:" 1>&2 ; usage 1>&2 ; exit -1 ; }

# Chainweb-node application
RUN=$1 && shift

# Number of nodes
N=$1 && shift

LOG_DIR=$1

# ############################################################################ #
# some sanity checks

# Check chainweb-node application
[ -x "$RUN" ] || { echo "chainweb-node \"$RUN\" can't be exectuted" 1>&1 ; usage 1>&2 ; exit -1 ; }

# check number of nodes
[ "$N" -ge 1 ] || { echo "number of nodes \"$N\" to small" 1>&2 ; exit -1 ; }
[ "$N" -le 100 ] || { echo "number of nodes \"$N\" to big" 1>&2 ; exit -1 ; }

# check logdir
[ -z "$LOG_DIR" -o -d "$LOG_DIR" ] || { echo "log directory \"$LOG_DIR\" doesn't exist" 1>&2 ; usage 1>&2 ; exit -1 ; }

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
    local PORT_ARG=$2
    local PEERID_ARG=$3

    if [[ -n "$LOG_DIR" ]] ; then

        # Run with LOG_DIR
        $RUN \
            --node-id=$NID \
            --mean-block-time=$((10 * N)) \
            --hostname=127.0.0.1 \
            --log-level=$LOGLEVEL \
            --cuts-logger-backend-handle="file:$LOG_DIR/cuts.node$NID.log" \
            --logger-backend-handle="file:$LOG_DIR/node$NID.log" \
            $PORT_ARG \
            $PEERID_ARG &
    else

        # Run without LOG_DIR
        $RUN \
            --node-id=$NID \
            --mean-block-time=$((10 * N)) \
            --hostname=127.0.0.1 \
            --log-level=$LOGLEVEL \
            $PORT_ARG \
            $PEERID_ARG &
    fi
}

echo "starting $N chainweb nodes"

# Start P2P bootstrap node
#
# a bootstrap node is a node with a well defined peer-info (peer-id and
# hostaddress) that is know to all other nodes on startup. For the Test
# chainweb-node application the bootstrap node peer-info is compiled
# into the initial peer-database.

run-node 0 --port=1789 --peer-id=525ff65f-9240-4ada-9c36-fe7da982b4b4
echo "started bootstrap node 0"

# Start remaining nodes
#
# When no peer-id is configured a random peer-id is generated on startup.
# Omitting the port argument is the same as using --port=0, which means 
# that a some free port is assigned to the node.

for ((i=1; i<N; i++)) ; do
    sleep 0.2
    run-node $i "" ""
    echo "started node $i"
done

wait


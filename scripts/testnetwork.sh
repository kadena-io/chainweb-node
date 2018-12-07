#! /bin/sh
node=dist/build/chainweb-node/chainweb-node
$node --port 45000 --telemetry-port 8000 --peerid d6904feb-e51b-4f0c-ac53-9e1b8284af3c & 
sleep 1
$node --port 45001 --telemetry-port 8001 --remote-peerid d6904feb-e51b-4f0c-ac53-9e1b8284af3c &
sleep 1
$node --port 45002 --telemetry-port 8002 --remote-peerid d6904feb-e51b-4f0c-ac53-9e1b8284af3c --peerid d6904feb-e51b-4f0c-ac53-aaaabbbbcccc &
sleep 1
$node --port 45003 --telemetry-port 8003 --remote-peerid d6904feb-e51b-4f0c-ac53-aaaabbbbcccc &
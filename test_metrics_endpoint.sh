#!/bin/bash

# Simple test script for the metrics endpoint
# This script tests the /metrics endpoint functionality

echo "Testing Chainweb Metrics Endpoint"
echo "================================="

# Test if the service is running (assuming default service port 1848)
SERVICE_PORT=${1:-1848}
SERVICE_HOST=${2:-localhost}
METRICS_URL="http://${SERVICE_HOST}:${SERVICE_PORT}/metrics"

echo "Testing metrics endpoint at: $METRICS_URL"
echo

# Test 1: Basic connectivity
echo "Test 1: Basic connectivity test..."
if curl -f -s -I "$METRICS_URL" > /dev/null 2>&1; then
    echo "✓ Metrics endpoint is accessible"
else
    echo "✗ Metrics endpoint is not accessible"
    echo "  Make sure chainweb-node is running with metrics enabled"
    exit 1
fi

# Test 2: Content-Type check
echo "Test 2: Content-Type verification..."
CONTENT_TYPE=$(curl -s -I "$METRICS_URL" | grep -i content-type | cut -d: -f2 | tr -d ' \r\n')
if [[ "$CONTENT_TYPE" == "text/plain"* ]]; then
    echo "✓ Correct Content-Type: $CONTENT_TYPE"
else
    echo "✗ Unexpected Content-Type: $CONTENT_TYPE"
fi

# Test 3: Prometheus format validation
echo "Test 3: Prometheus format validation..."
RESPONSE=$(curl -s "$METRICS_URL")

if echo "$RESPONSE" | grep -q "^# HELP"; then
    echo "✓ Response contains HELP comments"
else
    echo "✗ Response missing HELP comments"
fi

if echo "$RESPONSE" | grep -q "^# TYPE"; then
    echo "✓ Response contains TYPE declarations"
else
    echo "✗ Response missing TYPE declarations"
fi

if echo "$RESPONSE" | grep -q "chainweb_"; then
    echo "✓ Response contains chainweb metrics"
else
    echo "✗ Response missing chainweb metrics"
fi

# Test 4: Performance test
echo "Test 4: Performance test (5 requests)..."
for i in {1..5}; do
    START_TIME=$(date +%s%N)
    curl -s "$METRICS_URL" > /dev/null
    END_TIME=$(date +%s%N)
    DURATION=$((($END_TIME - $START_TIME) / 1000000))  # Convert to milliseconds
    echo "  Request $i: ${DURATION}ms"
done

# Test 5: Cache behavior test
echo "Test 5: Cache behavior test..."
echo "  First request (cache miss):"
time curl -s "$METRICS_URL" > /dev/null
echo "  Second request (cache hit):"
time curl -s "$METRICS_URL" > /dev/null

echo
echo "Testing completed!"
echo "For detailed output, run: curl $METRICS_URL"
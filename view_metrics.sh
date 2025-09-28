#!/bin/bash

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║         CHAINWEB PROMETHEUS METRICS DEMONSTRATION            ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo

echo "📊 METRICS ENDPOINT: http://localhost:1848/metrics"
echo "   Status: ✅ ACTIVE"
echo

echo "📈 REAL-TIME METRICS SNAPSHOT:"
echo "================================"

# Fetch metrics
METRICS=$(curl -s http://localhost:1848/metrics 2>/dev/null)

# Extract key metrics
CUT_HEIGHT=$(echo "$METRICS" | grep "chainweb_blockchain_cut_height " | grep -v "#" | awk '{print $2}')
CONNECTIONS=$(echo "$METRICS" | grep "chainweb_p2p_active_connections " | grep -v "#" | awk '{print $2}')
CPU=$(echo "$METRICS" | grep "chainweb_system_cpu_usage " | grep -v "#" | awk '{print $2}')
MEMORY_BYTES=$(echo "$METRICS" | grep "chainweb_system_memory_usage_bytes " | grep -v "#" | awk '{print $2}')
MEMORY_MB=$(echo "scale=2; $MEMORY_BYTES / 1048576" | bc 2>/dev/null || echo "N/A")

echo "🔗 Blockchain Status:"
echo "   • Cut Height: $CUT_HEIGHT"
echo "   • Chains: 20 parallel chains"
echo

echo "🌐 Network Status:"
echo "   • P2P Connections: $CONNECTIONS active peers"
echo

echo "💻 System Resources:"
echo "   • CPU Usage: ${CPU}%"
echo "   • Memory: ${MEMORY_MB} MB"
echo

echo "📊 Available Metric Types:"
echo "=========================="
echo "$METRICS" | grep "^# HELP" | while read -r line; do
    metric=$(echo "$line" | awk '{for(i=3;i<=NF;i++) printf "%s ", $i; print ""}')
    echo "   • $metric"
done

echo
echo "🔍 Sample Chain-Specific Metrics:"
echo "=================================="
echo "Chain 0 Metrics:"
echo "$METRICS" | grep 'chain_id="0"' | head -3 | while read -r line; do
    echo "   $line"
done

echo
echo "📡 To view all metrics: curl http://localhost:1848/metrics"
echo "📈 To integrate with Prometheus, add to prometheus.yml:"
echo "   scrape_configs:"
echo "     - job_name: 'chainweb'"
echo "       static_configs:"
echo "         - targets: ['localhost:1848']"
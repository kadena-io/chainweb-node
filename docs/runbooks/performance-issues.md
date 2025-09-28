# Performance Issues Runbook

This runbook provides investigation steps and resolution procedures for Chainweb performance-related alerts.

## Table of Contents

- [High P2P Latency](#high-p2p-latency)
- [Mempool Saturation](#mempool-saturation)
- [Validation Latency](#validation-latency)
- [Pact Execution Latency](#pact-execution-latency)
- [Connection Failures](#connection-failures)
- [Slow Cut Advancement](#slow-cut-advancement)
- [Slow Block Processing](#slow-block-processing)
- [Propagation Delay](#propagation-delay)
- [Task Queue Depth](#task-queue-depth)

---

## High P2P Latency

### Alert: ChainwebHighP2PLatency / ChainwebModerateP2PLatency

**Severity:** Critical / Warning
**Description:** P2P request latency exceeding acceptable thresholds (>1s critical, >500ms warning).

### Initial Investigation

1. **Check current latency metrics:**
   ```bash
   curl -s http://localhost:1848/metrics | grep chainweb_p2p_request_duration_seconds
   ```

2. **Test network connectivity to peers:**
   ```bash
   # Get active peers
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq '.peers[] | .address'

   # Test latency to each peer
   for peer in $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq -r '.peers[].address.hostname'); do
     echo "Testing $peer:"
     ping -c 5 $peer | tail -n 1
   done
   ```

3. **Check system resource usage:**
   ```bash
   # CPU and memory usage
   top -p $(pgrep chainweb-node) -n 1

   # Network interface statistics
   cat /proc/net/dev | grep -E "(eth|en)"

   # Socket statistics
   ss -tuln | grep :1789
   ```

### Common Root Causes

#### 1. Network Congestion
- **Symptoms:** High latency to all or most peers
- **Investigation:**
  ```bash
  # Check bandwidth utilization
  iftop -i eth0 -t -s 10

  # Monitor network errors
  cat /proc/net/dev | awk '{print $1, $4, $12}' | column -t

  # Check for packet loss
  for peer in $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq -r '.peers[].address.hostname[:5]'); do
    mtr -r -c 10 $peer
  done
  ```

#### 2. CPU Bottlenecks
- **Symptoms:** High CPU usage correlating with latency spikes
- **Investigation:**
  ```bash
  # Monitor CPU usage by thread
  top -H -p $(pgrep chainweb-node)

  # Check for CPU throttling
  dmesg | grep -i "cpu.*throttl"

  # Profile CPU usage
  perf top -p $(pgrep chainweb-node) -d 1
  ```

#### 3. Memory Pressure
- **Symptoms:** Increasing latency with high memory usage
- **Investigation:**
  ```bash
  # Check memory statistics
  cat /proc/meminfo | grep -E "(MemTotal|MemAvailable|MemFree)"

  # Monitor swap usage
  free -h
  swapon --show

  # Check for memory pressure
  cat /proc/pressure/memory
  ```

#### 4. Poor Peer Selection
- **Symptoms:** Consistently slow peers, geographic issues
- **Investigation:**
  ```bash
  # Analyze peer geographic distribution
  curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq '.peers[] | {hostname: .address.hostname, country: .address.country}'

  # Check peer quality scores
  curl -s http://localhost:1848/metrics | grep chainweb_p2p_peer_quality_score
  ```

### Resolution Steps

#### For Network Issues:
1. **Optimize network settings:**
   ```bash
   # Increase network buffer sizes
   echo 'net.core.rmem_max = 134217728' >> /etc/sysctl.conf
   echo 'net.core.wmem_max = 134217728' >> /etc/sysctl.conf
   echo 'net.ipv4.tcp_rmem = 4096 87380 134217728' >> /etc/sysctl.conf
   echo 'net.ipv4.tcp_wmem = 4096 65536 134217728' >> /etc/sysctl.conf
   sysctl -p
   ```

2. **Restart networking:**
   ```bash
   systemctl restart chainweb-node
   ```

#### For CPU Issues:
1. **Increase CPU priority:**
   ```bash
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Nice=-10
   # IOSchedulingClass=1
   # IOSchedulingPriority=4
   systemctl restart chainweb-node
   ```

2. **Enable CPU affinity:**
   ```bash
   # Bind to specific CPU cores
   taskset -cp 0-7 $(pgrep chainweb-node)
   ```

#### For Memory Issues:
1. **Increase memory limits:**
   ```bash
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_MEMORY_LIMIT=16G
   # MemoryLimit=20G
   systemctl restart chainweb-node
   ```

2. **Optimize GC settings:**
   ```bash
   # Add GC tuning options
   export CHAINWEB_GHC_RTS_OPTS="+RTS -A32M -n2m -qg -RTS"
   systemctl restart chainweb-node
   ```

#### For Peer Issues:
1. **Refresh peer list:**
   ```bash
   # Force peer discovery refresh
   curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/cut/peer/refresh
   ```

2. **Configure preferred peers:**
   ```bash
   # Edit configuration to prefer closer peers
   vim /etc/chainweb/config.yaml
   # Add peer preferences based on geography/latency
   systemctl restart chainweb-node
   ```

### Escalation Criteria
- P2P latency >5s consistently for >10 minutes
- Complete network isolation (no responsive peers)
- System resource exhaustion preventing recovery

---

## Mempool Saturation

### Alert: ChainwebMempoolSaturation / ChainwebMempoolHighUtilization

**Severity:** Critical / Warning
**Description:** Mempool usage exceeding capacity thresholds (>90% critical, >70% warning).

### Initial Investigation

1. **Check mempool status:**
   ```bash
   # Current mempool size and capacity
   curl -s http://localhost:1848/metrics | grep -E "chainweb_mempool_(current_size|max_capacity)"

   # Pending transactions per chain
   for chain in {0..19}; do
     echo "Chain $chain:"
     curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$chain/mempool/getPending" | jq '. | length'
   done
   ```

2. **Analyze transaction patterns:**
   ```bash
   # Check transaction insertion/rejection rates
   curl -s http://localhost:1848/metrics | grep -E "chainweb_mempool_transactions_(inserted|rejected)_total"

   # Look at transaction fees
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/0/mempool/getPending" | jq '.[] | .gasPrice' | sort -n
   ```

3. **Check processing rates:**
   ```bash
   # Block production rate
   curl -s http://localhost:1848/metrics | grep chainweb_blockchain_blocks_processed_total

   # Mempool processing duration
   curl -s http://localhost:1848/metrics | grep chainweb_mempool_validation_duration_seconds
   ```

### Common Root Causes

#### 1. High Transaction Volume
- **Symptoms:** Consistent high insertion rate, normal processing
- **Investigation:**
  ```bash
  # Monitor transaction arrival rate
  watch -n 1 "curl -s http://localhost:1848/metrics | grep chainweb_mempool_transactions_inserted_total"

  # Check transaction sources
  netstat -an | grep :1848 | awk '{print $5}' | cut -d: -f1 | sort | uniq -c | sort -nr
  ```

#### 2. Slow Transaction Processing
- **Symptoms:** Normal insertion rate, growing backlog
- **Investigation:**
  ```bash
  # Check validation latency
  curl -s http://localhost:1848/metrics | grep chainweb_mempool_validation_duration_seconds_bucket

  # Monitor block processing time
  curl -s http://localhost:1848/metrics | grep chainweb_blockchain_block_insertion_duration_seconds
  ```

#### 3. Fee-Related Issues
- **Symptoms:** High rejection rate, stuck low-fee transactions
- **Investigation:**
  ```bash
  # Analyze fee distribution
  curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/0/mempool/getPending" | jq '.[] | .gasPrice' | awk '{sum+=$1; count++} END {print "Average:", sum/count, "Min:", min, "Max:", max}'

  # Check rejection reasons
  tail -n 1000 /var/log/chainweb/chainweb-node.log | grep -i "reject\|invalid"
  ```

### Resolution Steps

#### For High Volume:
1. **Increase mempool capacity:**
   ```bash
   # Edit configuration
   vim /etc/chainweb/config.yaml
   # Increase mempool size limits
   systemctl restart chainweb-node
   ```

2. **Implement rate limiting:**
   ```bash
   # Configure connection limits
   iptables -A INPUT -p tcp --dport 1848 -m connlimit --connlimit-above 10 -j REJECT
   ```

#### For Processing Issues:
1. **Optimize validation:**
   ```bash
   # Increase validation workers
   export CHAINWEB_VALIDATION_WORKERS=8
   systemctl restart chainweb-node
   ```

2. **Tune Pact execution:**
   ```bash
   # Reduce execution timeouts for better throughput
   export CHAINWEB_PACT_EXECUTION_TIMEOUT=30
   systemctl restart chainweb-node
   ```

#### For Fee Issues:
1. **Configure fee thresholds:**
   ```bash
   # Set minimum fee requirements
   curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/mempool/config \
     -d '{"minGasPrice": 0.00001}'
   ```

2. **Clean low-fee transactions:**
   ```bash
   # Trigger mempool cleanup
   curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/mempool/cleanup
   ```

### Escalation Criteria
- Mempool at 100% capacity for >5 minutes
- Transaction processing completely stalled
- System memory exhaustion due to mempool growth

---

## Validation Latency

### Alert: ChainwebHighValidationLatency

**Severity:** Critical
**Description:** Transaction validation latency exceeding 5 seconds (95th percentile).

### Initial Investigation

1. **Check validation metrics:**
   ```bash
   curl -s http://localhost:1848/metrics | grep chainweb_mempool_validation_duration_seconds_bucket
   ```

2. **Analyze validation patterns:**
   ```bash
   # Check for specific transaction types causing issues
   tail -n 1000 /var/log/chainweb/chainweb-node.log | grep -i "validation" | grep -E "(slow|timeout|error)"
   ```

3. **Monitor system resources during validation:**
   ```bash
   # CPU usage during validation spikes
   pidstat -u -p $(pgrep chainweb-node) 1 10

   # Memory allocation patterns
   cat /proc/$(pgrep chainweb-node)/status | grep -E "VmRSS|VmSize"
   ```

### Resolution Steps

1. **Increase validation resources:**
   ```bash
   export CHAINWEB_VALIDATION_THREADS=16
   systemctl restart chainweb-node
   ```

2. **Optimize Pact execution:**
   ```bash
   # Tune Pact VM settings
   export CHAINWEB_PACT_REPL_CACHE_SIZE=1000
   export CHAINWEB_PACT_MODULE_CACHE_SIZE=500
   systemctl restart chainweb-node
   ```

3. **Implement validation caching:**
   ```bash
   # Enable signature verification caching
   export CHAINWEB_SIG_CACHE_SIZE=10000
   systemctl restart chainweb-node
   ```

---

## Pact Execution Latency

### Alert: ChainwebHighPactExecutionLatency

**Severity:** Critical
**Description:** Pact execution latency exceeding 10 seconds (95th percentile).

### Initial Investigation

1. **Check Pact execution metrics:**
   ```bash
   curl -s http://localhost:1848/metrics | grep chainweb_mempool_pact_execution_duration_seconds
   ```

2. **Analyze smart contract complexity:**
   ```bash
   # Look for complex transactions
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/0/mempool/getPending" | jq '.[] | {hash: .hash, codeLength: (.cmd | length)}'
   ```

3. **Check for runaway contracts:**
   ```bash
   tail -n 1000 /var/log/chainweb/chainweb-node.log | grep -i "pact.*timeout\|gas.*limit"
   ```

### Resolution Steps

1. **Tune execution limits:**
   ```bash
   # Reduce gas limits for better responsiveness
   export CHAINWEB_PACT_GAS_LIMIT=150000
   systemctl restart chainweb-node
   ```

2. **Implement execution monitoring:**
   ```bash
   # Enable detailed Pact logging
   export CHAINWEB_PACT_LOG_LEVEL=DEBUG
   systemctl restart chainweb-node
   ```

---

## Connection Failures

### Alert: ChainwebHighConnectionFailureRate

**Severity:** Critical
**Description:** P2P connection failure rate exceeding 30%.

### Initial Investigation

1. **Check connection statistics:**
   ```bash
   curl -s http://localhost:1848/metrics | grep -E "chainweb_p2p_(connection_failures|session_successes)_total"
   ```

2. **Test connectivity to known peers:**
   ```bash
   # Test TCP connectivity
   for peer in $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq -r '.peers[].address.hostname'); do
     nc -z -w5 $peer 1789 && echo "$peer: OK" || echo "$peer: FAIL"
   done
   ```

3. **Check for network issues:**
   ```bash
   # Monitor network interface errors
   cat /proc/net/dev | awk '{if(NR>2)print $1,$4,$12}' | column -t

   # Check firewall rules
   iptables -L -n | grep 1789
   ```

### Resolution Steps

1. **Optimize connection handling:**
   ```bash
   # Increase connection limits
   ulimit -n 65536
   echo "chainweb soft nofile 65536" >> /etc/security/limits.conf
   echo "chainweb hard nofile 65536" >> /etc/security/limits.conf
   ```

2. **Improve peer discovery:**
   ```bash
   # Refresh bootstrap peer list
   curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/cut/peer/refresh
   ```

---

## Quick Reference Scripts

### Performance Health Check
```bash
#!/bin/bash
# chainweb-performance-check.sh

echo "=== Chainweb Performance Health Check ==="

echo "P2P Latency (95th percentile):"
curl -s http://localhost:1848/metrics | grep chainweb_p2p_request_duration_seconds | grep "quantile=\"0.95\""

echo -e "\nMempool Utilization:"
current=$(curl -s http://localhost:1848/metrics | grep chainweb_mempool_current_size | awk '{print $2}')
capacity=$(curl -s http://localhost:1848/metrics | grep chainweb_mempool_max_capacity | awk '{print $2}')
echo "Scale(2); $current * 100 / $capacity" | bc -l | awk '{printf "%.2f%%\n", $1}'

echo -e "\nConnection Success Rate:"
failures=$(curl -s http://localhost:1848/metrics | grep chainweb_p2p_connection_failures_total | awk '{print $2}')
successes=$(curl -s http://localhost:1848/metrics | grep chainweb_p2p_session_successes_total | awk '{print $2}')
total=$((failures + successes))
if [ $total -gt 0 ]; then
  echo "scale=2; $successes * 100 / $total" | bc -l | awk '{printf "%.2f%%\n", $1}'
else
  echo "No data"
fi
```

### Performance Tuning Script
```bash
#!/bin/bash
# chainweb-performance-tune.sh

echo "Applying performance optimizations..."

# Network tuning
sysctl -w net.core.rmem_max=134217728
sysctl -w net.core.wmem_max=134217728
sysctl -w net.ipv4.tcp_rmem="4096 87380 134217728"
sysctl -w net.ipv4.tcp_wmem="4096 65536 134217728"

# File descriptor limits
ulimit -n 65536

# CPU affinity (adjust core count as needed)
taskset -cp 0-7 $(pgrep chainweb-node)

# Process priority
renice -10 $(pgrep chainweb-node)

echo "Performance tuning applied"
```

---

## Preventive Measures

1. **Regular monitoring:**
   - Set up performance dashboards
   - Monitor key latency metrics
   - Track throughput trends

2. **Capacity planning:**
   - Monitor resource utilization trends
   - Plan for traffic growth
   - Implement auto-scaling where possible

3. **Configuration optimization:**
   - Regularly review and tune configuration
   - Test performance changes in staging
   - Document optimal settings

4. **Network optimization:**
   - Maintain good peer connectivity
   - Monitor geographic distribution
   - Implement connection pooling

---

## Related Documentation

- [Consensus Failures Runbook](consensus-failures.md)
- [Resource Alerts Runbook](resource-alerts.md)
- [Database Issues Runbook](database-issues.md)
- [Chainweb Configuration Guide](https://github.com/kadena-io/chainweb-node/blob/master/README.md)
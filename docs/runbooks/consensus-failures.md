# Consensus Failures Runbook

This runbook provides investigation steps and resolution procedures for Chainweb consensus-related alerts.

## Table of Contents

- [Cut Stagnation](#cut-stagnation)
- [Chain Divergence](#chain-divergence)
- [Block Stagnation](#block-stagnation)
- [High Orphan Rate](#high-orphan-rate)
- [Consensus Transitions](#consensus-transitions)
- [Minor Divergence](#minor-divergence)
- [Inactive Chains](#inactive-chains)
- [Consistency Monitoring](#consistency-monitoring)

---

## Cut Stagnation

### Alert: ChainwebCutHeightStagnation

**Severity:** Critical
**Description:** Cut height has not advanced for more than 5 minutes, indicating complete consensus stagnation.

### Initial Investigation

1. **Check cut height across all chains:**
   ```bash
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '.hashes | to_entries | map({chain: .key, height: .value.height})'
   ```

2. **Verify node connectivity:**
   ```bash
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq '.peers | length'
   ```

3. **Check recent blocks:**
   ```bash
   # Check if any blocks have been mined recently
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/chain/0/header?limit=5 | jq '.items[0].creationTime'
   ```

### Common Root Causes

#### 1. Network Partitioning
- **Symptoms:** Low peer count, no recent blocks
- **Investigation:**
  ```bash
  # Check peer connectivity
  curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq '.peers[] | .address'

  # Test network connectivity to known peers
  for peer in $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq -r '.peers[].address.hostname'); do
    echo "Testing $peer"
    nc -z -w5 $peer 1789
  done
  ```

#### 2. Mining Pool Issues
- **Symptoms:** Cut stagnation with normal network connectivity
- **Investigation:**
  ```bash
  # Check mining coordination status
  curl -s http://localhost:1848/chainweb/0.0/mainnet01/mining/solved | jq '.'

  # Verify work requests
  curl -s http://localhost:1848/chainweb/0.0/mainnet01/mining/work | jq '.target'
  ```

#### 3. Database Corruption
- **Symptoms:** Errors in logs, slow responses
- **Investigation:**
  ```bash
  # Check database health
  cwtools db block-header-db consistency-check --config /path/to/config.yaml

  # Verify recent operations
  tail -n 100 /var/log/chainweb/chainweb-node.log | grep -i error
  ```

### Resolution Steps

#### For Network Issues:
1. **Restart networking:**
   ```bash
   systemctl restart chainweb-node
   ```

2. **Update peer list:**
   ```bash
   # Add bootstrap peers to configuration
   vim /etc/chainweb/config.yaml
   systemctl restart chainweb-node
   ```

#### For Mining Issues:
1. **Check mining coordination:**
   ```bash
   # Verify mining pool configuration
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/mining/config
   ```

2. **Restart mining services:**
   ```bash
   systemctl restart mining-pool
   ```

#### For Database Issues:
1. **Database repair:**
   ```bash
   # Stop node
   systemctl stop chainweb-node

   # Run database repair
   cwtools db block-header-db repair --config /path/to/config.yaml

   # Restart node
   systemctl start chainweb-node
   ```

### Escalation Criteria
- Cut stagnation persists for >15 minutes
- Database corruption detected
- Network-wide consensus failure

### Emergency Contacts
- Chainweb Dev Team: chainweb-dev@kadena.io
- Infrastructure Team: infrastructure@kadena.io

---

## Chain Divergence

### Alert: ChainwebChainHeightDivergence

**Severity:** Critical
**Description:** Significant height difference (>10 blocks) between chains, indicating potential consensus issues.

### Initial Investigation

1. **Identify divergent chains:**
   ```bash
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '.hashes | to_entries | map({chain: .key, height: .value.height}) | sort_by(.height)'
   ```

2. **Check chain synchronization status:**
   ```bash
   # For each chain, check recent block times
   for chain in {0..19}; do
     echo "Chain $chain:"
     curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$chain/header?limit=1" | jq '.items[0].creationTime'
   done
   ```

3. **Verify network consensus:**
   ```bash
   # Compare with other nodes
   for node in node1.example.com node2.example.com; do
     echo "Node $node:"
     curl -s "http://$node:1848/chainweb/0.0/mainnet01/cut" | jq '.height'
   done
   ```

### Common Root Causes

#### 1. Chain-Specific Mining Issues
- **Symptoms:** One or few chains lagging significantly
- **Investigation:**
  ```bash
  # Check chain difficulty
  curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$CHAIN_ID/header?limit=1" | jq '.items[0].target'

  # Verify chain-specific mining activity
  curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$CHAIN_ID/header?limit=10" | jq '.items[] | .creationTime'
  ```

#### 2. Selective Network Partitioning
- **Symptoms:** Consistent pattern of chain lag
- **Investigation:**
  ```bash
  # Check peer distribution across chains
  curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq '.peers[] | {address: .address, chains: .chains}'
  ```

#### 3. Resource Constraints
- **Symptoms:** Generally slow processing, gradual divergence
- **Investigation:**
  ```bash
  # Check system resources
  top -p $(pgrep chainweb-node)
  iostat -x 1 5
  ```

### Resolution Steps

#### For Mining Issues:
1. **Restart mining for specific chains:**
   ```bash
   # Check mining work distribution
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/mining/work | jq '.chainId'
   ```

#### For Network Issues:
1. **Force peer refresh:**
   ```bash
   # Restart with fresh peer discovery
   systemctl restart chainweb-node
   ```

#### For Resource Issues:
1. **Scale resources:**
   ```bash
   # Increase memory limits
   systemctl edit chainweb-node
   # Add: Environment=CHAINWEB_MEMORY_LIMIT=16G
   systemctl restart chainweb-node
   ```

### Escalation Criteria
- Divergence >20 blocks persists for >30 minutes
- Multiple chains affected simultaneously
- Evidence of network attack or split

---

## Block Stagnation

### Alert: ChainwebBlockHeightStagnation

**Severity:** Critical
**Description:** Individual chain block height has not increased in 10 minutes.

### Initial Investigation

1. **Check specific chain status:**
   ```bash
   CHAIN_ID=0  # Replace with affected chain
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$CHAIN_ID/header?limit=5" | jq '.items[] | {height: .height, time: .creationTime}'
   ```

2. **Verify mining activity:**
   ```bash
   # Check if work is being requested for this chain
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/mining/work" | jq '. | select(.chainId == "'$CHAIN_ID'")'
   ```

3. **Check mempool status:**
   ```bash
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$CHAIN_ID/mempool/getPending" | jq '. | length'
   ```

### Resolution Steps

1. **Force chain resync:**
   ```bash
   # Trigger cut update
   curl -X PUT http://localhost:1848/chainweb/0.0/mainnet01/cut
   ```

2. **Check and restart mining:**
   ```bash
   systemctl restart mining-coordinator
   ```

3. **Database consistency check:**
   ```bash
   cwtools db block-header-db consistency-check --chain $CHAIN_ID --config /path/to/config.yaml
   ```

---

## High Orphan Rate

### Alert: ChainwebHighOrphanBlockRate

**Severity:** Critical
**Description:** Orphan block rate >10%, indicating potential forks or consensus instability.

### Initial Investigation

1. **Analyze orphan block pattern:**
   ```bash
   # Check orphan blocks over time
   curl -s http://localhost:1848/metrics | grep chainweb_blockchain_orphan_blocks_total
   ```

2. **Investigate recent forks:**
   ```bash
   # Check for multiple blocks at same height
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$CHAIN_ID/header?limit=20" | jq '.items | group_by(.height) | map(select(length > 1))'
   ```

3. **Verify network timing:**
   ```bash
   # Check block time distribution
   curl -s "http://localhost:1848/chainweb/0.0/mainnet01/chain/$CHAIN_ID/header?limit=100" | jq '.items[] | .creationTime' | sort | uniq -c
   ```

### Common Root Causes

#### 1. Network Latency Issues
- **Investigation:**
  ```bash
  # Test latency to peers
  for peer in $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq -r '.peers[].address.hostname'); do
    ping -c 5 $peer
  done
  ```

#### 2. Mining Pool Conflicts
- **Investigation:**
  ```bash
  # Check for multiple mining sources
  netstat -an | grep :1789 | wc -l
  ```

#### 3. Clock Synchronization
- **Investigation:**
  ```bash
  # Verify NTP sync
  chrony sources -v
  timedatectl status
  ```

### Resolution Steps

1. **Improve network connectivity:**
   ```bash
   # Optimize network settings
   echo 'net.core.rmem_max = 134217728' >> /etc/sysctl.conf
   sysctl -p
   ```

2. **Synchronize clocks:**
   ```bash
   systemctl restart chronyd
   chrony makestep
   ```

---

## Consensus Transitions

### Alert: ChainwebAbnormalConsensusTransitions

**Severity:** Critical
**Description:** High rate of consensus state transitions indicating rapid fork switching.

### Initial Investigation

1. **Monitor transition patterns:**
   ```bash
   curl -s http://localhost:1848/metrics | grep chainweb_blockchain_consensus_state_transitions_total
   ```

2. **Check for competing forks:**
   ```bash
   # Look for rapid height changes
   watch -n 1 "curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '.height'"
   ```

### Resolution Steps

1. **Stabilize consensus:**
   ```bash
   # Reduce mining aggressiveness temporarily
   curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/mining/config -d '{"throttle": 0.5}'
   ```

2. **Isolate and restart if needed:**
   ```bash
   systemctl stop chainweb-node
   sleep 30
   systemctl start chainweb-node
   ```

---

## Quick Reference Commands

### Health Check Script
```bash
#!/bin/bash
# chainweb-consensus-health-check.sh

echo "=== Chainweb Consensus Health Check ==="
echo "Cut Height: $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '.height')"
echo "Peer Count: $(curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut/peer | jq '.peers | length')"

echo -e "\nChain Heights:"
curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '.hashes | to_entries | map({chain: .key, height: .value.height}) | sort_by(.height)'

echo -e "\nOrphan Rate (5min):"
curl -s http://localhost:1848/metrics | grep chainweb_blockchain_orphan_blocks_total | awk '{print $2}'
```

### Emergency Stop Procedure
```bash
#!/bin/bash
# emergency-stop.sh

echo "Emergency stop initiated at $(date)"
systemctl stop chainweb-node
systemctl stop mining-coordinator
echo "Services stopped. Manual intervention required."
```

### Logs Analysis
```bash
# Check for consensus-related errors
tail -f /var/log/chainweb/chainweb-node.log | grep -i "consensus\|fork\|orphan"

# Monitor cut advancement
tail -f /var/log/chainweb/chainweb-node.log | grep "cut.*advanced"
```

---

## Preventive Measures

1. **Regular monitoring:**
   - Set up comprehensive alerting
   - Monitor peer connectivity
   - Track block time distributions

2. **Infrastructure maintenance:**
   - Keep clocks synchronized
   - Maintain adequate resources
   - Regular backup verification

3. **Network optimization:**
   - Optimize peer selection
   - Monitor bandwidth usage
   - Implement connection pooling

---

## Related Documentation

- [Performance Issues Runbook](performance-issues.md)
- [Database Issues Runbook](database-issues.md)
- [Resource Alerts Runbook](resource-alerts.md)
- [Chainweb Protocol Documentation](https://github.com/kadena-io/chainweb-protocol)
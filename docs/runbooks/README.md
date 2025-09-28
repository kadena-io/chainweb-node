# Chainweb Node Runbooks

This directory contains operational runbooks for investigating and resolving common issues with Chainweb nodes. Each runbook provides step-by-step procedures for specific alert categories.

## Available Runbooks

### [Consensus Failures](consensus-failures.md)
**Scope:** Blockchain consensus and synchronization issues
- Cut height stagnation
- Chain height divergence
- Block processing stagnation
- High orphan block rates
- Consensus state transition anomalies
- Chain synchronization problems

**When to use:** When alerts indicate issues with blockchain consensus, chain synchronization, or cut advancement.

### [Performance Issues](performance-issues.md)
**Scope:** System and application performance problems
- High P2P message latency
- Mempool saturation and high utilization
- Transaction validation latency
- Pact execution performance
- Connection failures and timeouts
- Slow block processing and cut advancement

**When to use:** When alerts indicate degraded performance in networking, transaction processing, or general system responsiveness.

### [Resource Alerts](resource-alerts.md)
**Scope:** System resource exhaustion and management
- CPU usage and exhaustion
- Memory pressure and exhaustion
- Disk space management
- Garbage collection issues
- Thread count problems
- Emergency resource recovery

**When to use:** When alerts indicate system resource constraints that may impact node operation.

### [Database Issues](database-issues.md)
**Scope:** RocksDB performance and integrity problems
- Read and write latency issues
- Cache performance problems
- Compaction failures and delays
- Backup operation failures
- Database size anomalies
- Corruption detection and recovery

**When to use:** When alerts indicate database performance issues or data integrity concerns.

## Quick Reference

### Emergency Contacts
- **Chainweb Dev Team:** chainweb-dev@kadena.io
- **Infrastructure Team:** infrastructure@kadena.io
- **Security Team:** security@kadena.io

### Escalation Matrix

| Severity | Response Time | Escalation | Contact |
|----------|---------------|------------|---------|
| Critical | Immediate | 15 minutes | On-call engineer + Team lead |
| Warning | 30 minutes | 2 hours | On-call engineer |
| Info | 4 hours | 24 hours | During business hours |

### Common Tools and Commands

#### Health Check Commands
```bash
# Node status
curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '.height'

# Metrics overview
curl -s http://localhost:1848/metrics | grep chainweb_ | head -20

# Resource usage
top -p $(pgrep chainweb-node) -n 1

# Database health
cwtools db block-header-db stats --config /etc/chainweb/config.yaml
```

#### Emergency Commands
```bash
# Stop node safely
systemctl stop chainweb-node

# Emergency resource cleanup
echo 3 > /proc/sys/vm/drop_caches

# Force peer refresh
curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/cut/peer/refresh

# Database repair
cwtools db block-header-db repair --config /etc/chainweb/config.yaml
```

## Alert Mapping

### Critical Alerts → Runbook Sections

| Alert Name | Runbook | Section |
|------------|---------|---------|
| ChainwebCutHeightStagnation | [Consensus Failures](consensus-failures.md) | Cut Stagnation |
| ChainwebChainHeightDivergence | [Consensus Failures](consensus-failures.md) | Chain Divergence |
| ChainwebBlockHeightStagnation | [Consensus Failures](consensus-failures.md) | Block Stagnation |
| ChainwebHighOrphanBlockRate | [Consensus Failures](consensus-failures.md) | High Orphan Rate |
| ChainwebHighP2PLatency | [Performance Issues](performance-issues.md) | High P2P Latency |
| ChainwebMempoolSaturation | [Performance Issues](performance-issues.md) | Mempool Saturation |
| ChainwebHighValidationLatency | [Performance Issues](performance-issues.md) | Validation Latency |
| ChainwebHighCPUUsage | [Resource Alerts](resource-alerts.md) | CPU Exhaustion |
| ChainwebHighMemoryUsage | [Resource Alerts](resource-alerts.md) | Memory Exhaustion |
| ChainwebDiskSpaceExhaustion | [Resource Alerts](resource-alerts.md) | Disk Exhaustion |
| ChainwebRocksDBHighReadLatency | [Database Issues](database-issues.md) | Read Latency Issues |
| ChainwebRocksDBHighWriteLatency | [Database Issues](database-issues.md) | Write Latency Issues |

### Warning Alerts → Runbook Sections

| Alert Name | Runbook | Section |
|------------|---------|---------|
| ChainwebSlowCutAdvancement | [Consensus Failures](consensus-failures.md) | Minor Divergence |
| ChainwebModerateP2PLatency | [Performance Issues](performance-issues.md) | High P2P Latency |
| ChainwebMempoolHighUtilization | [Performance Issues](performance-issues.md) | Mempool Saturation |
| ChainwebElevatedCPUUsage | [Resource Alerts](resource-alerts.md) | CPU Exhaustion |
| ChainwebHighMemoryPressure | [Resource Alerts](resource-alerts.md) | Memory Exhaustion |
| ChainwebRocksDBCompactionDelays | [Database Issues](database-issues.md) | Compaction Problems |

## Investigation Workflow

### Step 1: Initial Assessment (2-5 minutes)
1. **Identify the alert severity and type**
2. **Check node basic health:**
   ```bash
   # Quick health check
   curl -s http://localhost:1848/chainweb/0.0/mainnet01/cut | jq '{height: .height, weight: .weight}'
   systemctl is-active chainweb-node
   ```
3. **Navigate to appropriate runbook**

### Step 2: Follow Runbook Procedures (5-30 minutes)
1. **Execute investigation steps from the relevant runbook**
2. **Document findings and actions taken**
3. **Apply resolution steps as appropriate**

### Step 3: Monitoring and Follow-up (Ongoing)
1. **Monitor alert status after resolution**
2. **Document lessons learned**
3. **Update runbooks if new procedures discovered**

## Troubleshooting Tips

### When Multiple Alerts Fire
1. **Prioritize by severity:** Critical → Warning → Info
2. **Look for root cause:** Often one issue causes cascading alerts
3. **Start with infrastructure:** Resource issues often cause performance issues

### When Runbook Steps Don't Work
1. **Check for recent changes:** Configuration, software updates, infrastructure
2. **Verify prerequisites:** Ensure tools and permissions are available
3. **Escalate quickly:** Don't spend >30 minutes on critical issues

### Communication During Incidents
1. **Create incident channel/ticket**
2. **Provide regular updates (every 15-30 minutes for critical issues)**
3. **Document all actions taken**
4. **Post-incident review for critical issues**

## Runbook Maintenance

### Regular Review Schedule
- **Monthly:** Review and update procedures based on recent incidents
- **Quarterly:** Full runbook review and testing
- **After incidents:** Update procedures based on lessons learned

### Testing Procedures
- **Chaos engineering:** Regularly test failure scenarios
- **Runbook drills:** Practice procedures during maintenance windows
- **Tool validation:** Ensure all referenced tools and commands work

### Contributing to Runbooks
1. **Document new procedures** discovered during incident resolution
2. **Update existing procedures** when better approaches are found
3. **Add monitoring improvements** based on operational experience
4. **Submit updates** via pull request with review

## Related Documentation

- [Monitoring and Alerting Guide](../monitoring/README.md)
- [Configuration Reference](../configuration/README.md)
- [Operational Procedures](../operations/README.md)
- [Security Procedures](../security/README.md)
- [Chainweb Protocol Documentation](https://github.com/kadena-io/chainweb-protocol)

## Appendix: Common Log Patterns

### Consensus Issues
```
ERROR: Cut advancement stalled
WARN: Chain height divergence detected
ERROR: Orphan block rate exceeded threshold
INFO: Consensus state transition
```

### Performance Issues
```
WARN: High P2P latency detected
ERROR: Mempool saturation reached
WARN: Validation taking longer than expected
ERROR: Connection timeout exceeded
```

### Resource Issues
```
WARN: High CPU usage sustained
ERROR: Memory allocation failed
CRIT: Disk space critically low
WARN: GC pause duration exceeded
```

### Database Issues
```
ERROR: RocksDB read timeout
WARN: Compaction falling behind
ERROR: Cache hit rate below threshold
CRIT: Database corruption detected
```
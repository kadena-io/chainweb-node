# Database Issues Runbook

This runbook provides investigation steps and resolution procedures for Chainweb database-related alerts, focusing on RocksDB performance and integrity issues.

## Table of Contents

- [Read Latency Issues](#read-latency-issues)
- [Write Latency Issues](#write-latency-issues)
- [Cache Hit Rate Issues](#cache-hit-rate-issues)
- [Compaction Problems](#compaction-problems)
- [Backup Failures](#backup-failures)
- [Database Size Anomalies](#database-size-anomalies)
- [Corruption Recovery](#corruption-recovery)

---

## Read Latency Issues

### Alert: ChainwebRocksDBHighReadLatency

**Severity:** Critical
**Description:** Database read operations taking >100ms (95th percentile), indicating serious performance issues.

### Initial Investigation

1. **Check read performance metrics:**
   ```bash
   # Current read latency statistics
   curl -s http://localhost:1848/metrics | grep chainweb_database_read_duration_seconds

   # Read operation rate
   curl -s http://localhost:1848/metrics | grep chainweb_database_reads_total

   # Read amplification
   curl -s http://localhost:1848/metrics | grep chainweb_database_read_amplification
   ```

2. **Analyze database statistics:**
   ```bash
   # RocksDB stats via cwtools
   cwtools db block-header-db stats --config /etc/chainweb/config.yaml

   # Check compaction status
   cwtools db block-header-db compaction-stats --config /etc/chainweb/config.yaml

   # Memory usage
   cwtools db block-header-db memory-usage --config /etc/chainweb/config.yaml
   ```

3. **System-level investigation:**
   ```bash
   # I/O statistics
   iostat -x 1 5

   # Disk latency
   iotop -p $(pgrep chainweb-node) -d 1 -n 5

   # Check for I/O wait
   vmstat 1 5
   ```

### Common Root Causes

#### 1. Poor Cache Performance
- **Symptoms:** High read latency with low cache hit rate
- **Investigation:**
  ```bash
  # Check cache hit rate
  curl -s http://localhost:1848/metrics | grep chainweb_database_cache_hit_rate

  # Cache size configuration
  cwtools db block-header-db cache-info --config /etc/chainweb/config.yaml

  # Memory pressure on cache
  cat /proc/$(pgrep chainweb-node)/status | grep VmRSS
  ```

#### 2. Compaction Issues
- **Symptoms:** High read amplification, multiple levels with overlapping files
- **Investigation:**
  ```bash
  # LSM tree structure
  cwtools db block-header-db level-info --config /etc/chainweb/config.yaml

  # Pending compactions
  cwtools db block-header-db pending-compactions --config /etc/chainweb/config.yaml

  # File count per level
  cwtools db block-header-db file-count --config /etc/chainweb/config.yaml
  ```

#### 3. Storage Hardware Issues
- **Symptoms:** High I/O latency across all operations
- **Investigation:**
  ```bash
  # Disk performance test
  fio --name=random-read --ioengine=libaio --rw=randread --bs=4k --numjobs=4 --size=1G --runtime=60 --group_reporting --filename=/var/lib/chainweb/test-file

  # SMART status
  smartctl -a /dev/sda

  # Check for disk errors
  dmesg | grep -i "error\|fail" | grep -i "sd\|nvme"
  ```

#### 4. Database Fragmentation
- **Symptoms:** Increasing read latency over time
- **Investigation:**
  ```bash
  # Database size vs actual data
  du -sh /var/lib/chainweb/*/rocksDb/
  cwtools db block-header-db size-info --config /etc/chainweb/config.yaml

  # File fragmentation
  find /var/lib/chainweb -name "*.sst" -exec ls -la {} \; | awk '{print $5}' | sort -n
  ```

### Resolution Steps

#### For Cache Issues:
1. **Increase cache size:**
   ```bash
   # Stop node
   systemctl stop chainweb-node

   # Update configuration
   vim /etc/chainweb/config.yaml
   # Increase block cache and table cache sizes

   # Restart node
   systemctl start chainweb-node
   ```

2. **Optimize cache configuration:**
   ```bash
   # Enable cache statistics
   cwtools db block-header-db configure-cache --cache-size 2G --config /etc/chainweb/config.yaml
   ```

#### For Compaction Issues:
1. **Force compaction:**
   ```bash
   # Stop node for maintenance
   systemctl stop chainweb-node

   # Manual compaction
   cwtools db block-header-db compact --config /etc/chainweb/config.yaml

   # Restart node
   systemctl start chainweb-node
   ```

2. **Optimize compaction settings:**
   ```bash
   # Update RocksDB options
   vim /etc/chainweb/config.yaml
   # Add compaction optimization settings
   systemctl restart chainweb-node
   ```

#### For Hardware Issues:
1. **Immediate mitigation:**
   ```bash
   # Reduce I/O load
   systemctl edit chainweb-node
   # Add: Environment=CHAINWEB_DB_THREADS=2
   systemctl restart chainweb-node
   ```

2. **Hardware intervention:**
   ```bash
   # Schedule hardware maintenance
   # Consider SSD upgrade or RAID configuration
   ```

### Escalation Criteria
- Read latency >1 second consistently
- Cache hit rate <70%
- Hardware errors detected
- Node unable to sync due to read performance

---

## Write Latency Issues

### Alert: ChainwebRocksDBHighWriteLatency

**Severity:** Critical
**Description:** Database write operations taking >500ms (95th percentile), severely impacting block processing.

### Initial Investigation

1. **Check write performance:**
   ```bash
   # Write latency metrics
   curl -s http://localhost:1848/metrics | grep chainweb_database_write_duration_seconds

   # Write operation rate
   curl -s http://localhost:1848/metrics | grep chainweb_database_writes_total

   # WAL sync latency
   curl -s http://localhost:1848/metrics | grep chainweb_database_wal_sync_duration
   ```

2. **Analyze write patterns:**
   ```bash
   # Batch write sizes
   curl -s http://localhost:1848/metrics | grep chainweb_database_batch_size

   # Write amplification
   curl -s http://localhost:1848/metrics | grep chainweb_database_write_amplification

   # Memtable flush frequency
   curl -s http://localhost:1848/metrics | grep chainweb_database_memtable_flushes
   ```

### Resolution Steps

#### For Write Amplification:
1. **Optimize compaction strategy:**
   ```bash
   systemctl stop chainweb-node
   vim /etc/chainweb/config.yaml
   # Configure level-based compaction with size ratios
   systemctl start chainweb-node
   ```

#### For WAL Issues:
1. **Optimize WAL settings:**
   ```bash
   # Configure WAL buffer size
   vim /etc/chainweb/config.yaml
   # Add WAL optimization settings
   systemctl restart chainweb-node
   ```

#### For Memtable Issues:
1. **Tune memtable configuration:**
   ```bash
   # Increase memtable size
   vim /etc/chainweb/config.yaml
   # Configure larger memtables with delayed flushes
   systemctl restart chainweb-node
   ```

---

## Cache Hit Rate Issues

### Alert: ChainwebRocksDBLowCacheHitRate

**Severity:** Critical
**Description:** Cache hit rate <80%, indicating insufficient cache allocation or cache thrashing.

### Initial Investigation

1. **Analyze cache performance:**
   ```bash
   # Cache hit/miss statistics
   curl -s http://localhost:1848/metrics | grep chainweb_database_cache

   # Cache size vs working set
   cwtools db block-header-db cache-info --config /etc/chainweb/config.yaml

   # Memory allocation for caches
   cat /proc/$(pgrep chainweb-node)/smaps | grep -A 5 -B 5 cache
   ```

2. **Check access patterns:**
   ```bash
   # Hot data identification
   cwtools db block-header-db hot-keys --config /etc/chainweb/config.yaml

   # Access pattern analysis
   tail -n 10000 /var/log/chainweb/chainweb-node.log | grep -i cache | head -20
   ```

### Resolution Steps

1. **Increase cache sizes:**
   ```bash
   systemctl stop chainweb-node
   vim /etc/chainweb/config.yaml
   # Increase block cache from 1GB to 4GB
   # Increase table cache appropriately
   systemctl start chainweb-node
   ```

2. **Optimize cache policies:**
   ```bash
   # Configure LRU cache with better policies
   vim /etc/chainweb/config.yaml
   # Add cache optimization settings
   systemctl restart chainweb-node
   ```

---

## Compaction Problems

### Alert: ChainwebRocksDBCompactionFailures / ChainwebRocksDBCompactionDelays

**Severity:** Critical / Warning
**Description:** Compaction operations failing or taking excessive time, leading to performance degradation.

### Initial Investigation

1. **Check compaction status:**
   ```bash
   # Compaction statistics
   cwtools db block-header-db compaction-stats --config /etc/chainweb/config.yaml

   # Pending compactions
   cwtools db block-header-db pending-compactions --config /etc/chainweb/config.yaml

   # Compaction duration trends
   curl -s http://localhost:1848/metrics | grep chainweb_database_compaction_duration_seconds
   ```

2. **Analyze LSM tree health:**
   ```bash
   # File count per level
   cwtools db block-header-db level-info --config /etc/chainweb/config.yaml

   # Size amplification
   cwtools db block-header-db size-amplification --config /etc/chainweb/config.yaml

   # Check for stuck compactions
   ps aux | grep compact
   ```

### Common Issues and Solutions

#### 1. Compaction Falling Behind
- **Resolution:**
  ```bash
  # Increase compaction threads
  vim /etc/chainweb/config.yaml
  # Add: compactionThreads: 4
  systemctl restart chainweb-node
  ```

#### 2. Disk Space Issues During Compaction
- **Resolution:**
  ```bash
  # Ensure adequate free space (2x database size)
  df -h /var/lib/chainweb

  # Clean up if necessary
  find /var/lib/chainweb -name "*.log" -mtime +7 -delete
  ```

#### 3. Memory Pressure During Compaction
- **Resolution:**
  ```bash
  # Limit compaction memory usage
  vim /etc/chainweb/config.yaml
  # Add memory limits for compaction
  systemctl restart chainweb-node
  ```

---

## Backup Failures

### Alert: ChainwebBackupOperationFailures

**Severity:** Critical
**Description:** Database backup operations failing, compromising data recovery capabilities.

### Initial Investigation

1. **Check backup status:**
   ```bash
   # Recent backup attempts
   tail -n 100 /var/log/chainweb/backup.log

   # Backup file integrity
   ls -la /var/lib/chainweb/backups/

   # Available space for backups
   df -h /var/lib/chainweb/backups/
   ```

2. **Test backup creation:**
   ```bash
   # Manual backup test
   cwtools db block-header-db backup --dest /tmp/test-backup --config /etc/chainweb/config.yaml
   ```

### Resolution Steps

1. **Fix common backup issues:**
   ```bash
   # Ensure backup directory exists and has permissions
   mkdir -p /var/lib/chainweb/backups
   chown chainweb:chainweb /var/lib/chainweb/backups

   # Clean old backups if space issue
   find /var/lib/chainweb/backups -name "*.tar.gz" -mtime +30 -delete
   ```

2. **Restore backup functionality:**
   ```bash
   # Restart backup service
   systemctl restart chainweb-backup

   # Test backup operation
   cwtools db block-header-db backup --config /etc/chainweb/config.yaml
   ```

---

## Database Size Anomalies

### Alert: ChainwebDatabaseSizeAnomalyGrowth

**Severity:** Critical
**Description:** Abnormal database growth rate indicating potential issues or attacks.

### Initial Investigation

1. **Analyze growth patterns:**
   ```bash
   # Current database size
   du -sh /var/lib/chainweb/*/rocksDb/

   # Growth rate analysis
   tail -n 1000 /var/log/chainweb/chainweb-node.log | grep -i "database.*size"

   # Compare with historical data
   ls -la /var/lib/chainweb/*/rocksDb/ | head -10
   ```

2. **Identify causes:**
   ```bash
   # Check for unusual transaction patterns
   curl -s http://localhost:1848/metrics | grep chainweb_mempool_transactions

   # Look for data retention issues
   cwtools db block-header-db retention-check --config /etc/chainweb/config.yaml
   ```

### Resolution Steps

1. **Investigate and mitigate:**
   ```bash
   # Enable detailed logging
   systemctl edit chainweb-node
   # Add: Environment=CHAINWEB_LOG_LEVEL=DEBUG
   systemctl restart chainweb-node

   # Monitor for unusual activity
   tail -f /var/log/chainweb/chainweb-node.log | grep -i "database\|size"
   ```

2. **Emergency size reduction:**
   ```bash
   # If confirmed as anomaly, compact database
   systemctl stop chainweb-node
   cwtools db block-header-db compact --config /etc/chainweb/config.yaml
   systemctl start chainweb-node
   ```

---

## Corruption Recovery

### Database Corruption Detection and Recovery

**When to suspect corruption:**
- Checksum errors in logs
- Unexpected read/write failures
- Node fails to start with database errors

### Recovery Procedures

1. **Backup current state:**
   ```bash
   # Stop node immediately
   systemctl stop chainweb-node

   # Backup corrupted database
   cp -r /var/lib/chainweb/*/rocksDb /var/lib/chainweb/corrupted-backup-$(date +%Y%m%d)
   ```

2. **Attempt repair:**
   ```bash
   # Try RocksDB repair
   cwtools db block-header-db repair --config /etc/chainweb/config.yaml

   # If repair fails, restore from backup
   rm -rf /var/lib/chainweb/*/rocksDb
   tar -xzf /var/lib/chainweb/backups/latest-backup.tar.gz -C /var/lib/chainweb/
   ```

3. **Full resync if necessary:**
   ```bash
   # Last resort: delete database and resync
   systemctl stop chainweb-node
   rm -rf /var/lib/chainweb/*/rocksDb
   systemctl start chainweb-node
   # Node will resync from peers
   ```

---

## Database Maintenance Scripts

### Health Check Script
```bash
#!/bin/bash
# db-health-check.sh

echo "=== Database Health Check ==="

# Check database sizes
echo "Database sizes:"
du -sh /var/lib/chainweb/*/rocksDb/

# Check cache hit rates
echo -e "\nCache hit rates:"
curl -s http://localhost:1848/metrics | grep chainweb_database_cache_hit_rate

# Check compaction status
echo -e "\nCompaction status:"
cwtools db block-header-db compaction-stats --config /etc/chainweb/config.yaml

# Check for errors
echo -e "\nRecent database errors:"
tail -n 1000 /var/log/chainweb/chainweb-node.log | grep -i "database.*error" | tail -5
```

### Performance Optimization Script
```bash
#!/bin/bash
# db-optimize.sh

echo "Optimizing database performance..."

# Stop node
systemctl stop chainweb-node

# Compact database
cwtools db block-header-db compact --config /etc/chainweb/config.yaml

# Update cache sizes based on available memory
TOTAL_MEM=$(free -g | awk '/^Mem:/{print $2}')
CACHE_SIZE=$((TOTAL_MEM / 4))

echo "Setting cache size to ${CACHE_SIZE}GB"

# Restart node
systemctl start chainweb-node

echo "Optimization complete"
```

---

## Preventive Measures

1. **Regular maintenance:**
   - Schedule periodic compactions
   - Monitor database growth trends
   - Regular backup verification

2. **Performance monitoring:**
   - Track read/write latency trends
   - Monitor cache hit rates
   - Alert on anomalous growth

3. **Configuration optimization:**
   - Regularly review and tune RocksDB settings
   - Adjust cache sizes based on workload
   - Optimize compaction strategies

4. **Backup strategy:**
   - Regular automated backups
   - Test backup restoration procedures
   - Monitor backup completion

---

## Related Documentation

- [RocksDB Tuning Guide](https://github.com/facebook/rocksdb/wiki/Tuning-Guide)
- [Resource Alerts Runbook](resource-alerts.md)
- [Performance Issues Runbook](performance-issues.md)
- [Chainweb Database Configuration](https://github.com/kadena-io/chainweb-node/blob/master/docs/database.md)
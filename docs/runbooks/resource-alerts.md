# Resource Alerts Runbook

This runbook provides investigation steps and resolution procedures for Chainweb system resource and infrastructure alerts.

## Table of Contents

- [CPU Exhaustion](#cpu-exhaustion)
- [Memory Exhaustion](#memory-exhaustion)
- [Disk Exhaustion](#disk-exhaustion)
- [GC Pauses](#gc-pauses)
- [Thread Count Issues](#thread-count-issues)
- [Emergency Cleanup](#emergency-cleanup)

---

## CPU Exhaustion

### Alert: ChainwebHighCPUUsage / ChainwebElevatedCPUUsage

**Severity:** Critical / Warning
**Description:** CPU usage exceeding thresholds (>90% critical, >70% warning).

### Initial Investigation

1. **Check current CPU usage:**
   ```bash
   # Overall system CPU usage
   top -n 1 | head -n 5

   # Chainweb-specific CPU usage
   top -p $(pgrep chainweb-node) -n 1

   # CPU usage by thread
   top -H -p $(pgrep chainweb-node) -n 1
   ```

2. **Identify CPU-intensive operations:**
   ```bash
   # Profile CPU usage for 30 seconds
   perf top -p $(pgrep chainweb-node) -d 1 --stdio | head -n 20

   # Check for specific hot functions
   perf record -p $(pgrep chainweb-node) -g sleep 10
   perf report --stdio | head -n 50
   ```

3. **Check system load and processes:**
   ```bash
   # System load averages
   uptime

   # Process tree
   pstree -p $(pgrep chainweb-node)

   # Check for other resource-intensive processes
   ps aux --sort=-%cpu | head -n 10
   ```

### Common Root Causes

#### 1. High Transaction Volume
- **Symptoms:** CPU spikes correlating with mempool activity
- **Investigation:**
  ```bash
  # Check transaction processing rate
  curl -s http://localhost:1848/metrics | grep chainweb_mempool_transactions_inserted_total

  # Monitor validation CPU usage
  pidstat -u -p $(pgrep chainweb-node) 1 10
  ```

#### 2. Inefficient Algorithms or Memory Pressure
- **Symptoms:** Consistently high CPU without proportional work
- **Investigation:**
  ```bash
  # Check memory allocation rate
  cat /proc/$(pgrep chainweb-node)/status | grep -E "VmRSS|VmSize|VmPeak"

  # Monitor GC activity
  curl -s http://localhost:1848/metrics | grep chainweb_system_gc_count_total
  ```

#### 3. Network I/O Issues
- **Symptoms:** High CPU during P2P operations
- **Investigation:**
  ```bash
  # Monitor network activity
  iftop -i eth0 -t -s 10

  # Check socket statistics
  ss -s

  # Monitor P2P specific metrics
  curl -s http://localhost:1848/metrics | grep chainweb_p2p_request_duration_seconds
  ```

#### 4. Database Operations
- **Symptoms:** CPU spikes during database operations
- **Investigation:**
  ```bash
  # Check database operation latency
  curl -s http://localhost:1848/metrics | grep chainweb_database.*duration_seconds

  # Monitor I/O wait time
  iostat -x 1 5 | grep -E "(Device|chainweb)"
  ```

### Resolution Steps

#### Immediate Actions (Critical):
1. **Reduce processing load:**
   ```bash
   # Temporarily reduce connection limits
   iptables -A INPUT -p tcp --dport 1789 -m connlimit --connlimit-above 5 -j REJECT
   iptables -A INPUT -p tcp --dport 1848 -m connlimit --connlimit-above 10 -j REJECT

   # Lower process priority for other services
   systemctl stop non-essential-service1
   systemctl stop non-essential-service2
   ```

2. **Increase CPU allocation:**
   ```bash
   # Set higher CPU priority
   renice -10 $(pgrep chainweb-node)

   # CPU affinity to use all available cores
   taskset -cp 0-$(nproc) $(pgrep chainweb-node)
   ```

#### Medium-term fixes:
1. **Optimize configuration:**
   ```bash
   # Reduce concurrent operations
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_WORKER_THREADS=4
   # Environment=CHAINWEB_VALIDATION_WORKERS=2
   systemctl restart chainweb-node
   ```

2. **Enable performance optimizations:**
   ```bash
   # Add performance flags
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_GHC_RTS_OPTS="+RTS -A32M -n2m -qg -H2G -RTS"
   systemctl restart chainweb-node
   ```

#### Long-term solutions:
1. **Scale horizontally:**
   ```bash
   # Plan for additional nodes
   # Document current performance limits
   # Implement load balancing
   ```

2. **Hardware upgrade:**
   ```bash
   # Monitor for sustained high usage
   # Plan CPU upgrade based on usage patterns
   ```

### Escalation Criteria
- CPU usage >95% for >10 minutes
- System becomes unresponsive
- Critical system processes affected

---

## Memory Exhaustion

### Alert: ChainwebHighMemoryUsage / ChainwebHighMemoryPressure

**Severity:** Critical / Warning
**Description:** Memory usage exceeding thresholds (>85% critical, >70% warning).

### Initial Investigation

1. **Check memory usage:**
   ```bash
   # Overall memory status
   free -h

   # Chainweb process memory usage
   ps -o pid,ppid,cmd,%mem,%cpu --sort=-%mem -p $(pgrep chainweb-node)

   # Detailed memory breakdown
   cat /proc/$(pgrep chainweb-node)/smaps | grep -E "^(Rss|Pss|Size|Private)" | awk '{sum+=$2} END {print sum " kB"}'
   ```

2. **Analyze memory pressure:**
   ```bash
   # Check for OOM killer activity
   dmesg | grep -i "killed process\|out of memory"

   # Memory pressure indicators
   cat /proc/pressure/memory

   # Swap usage
   cat /proc/swaps
   swapon --show
   ```

3. **Check for memory leaks:**
   ```bash
   # Monitor memory growth over time
   while true; do
     echo "$(date): $(cat /proc/$(pgrep chainweb-node)/status | grep VmRSS)"
     sleep 60
   done

   # GC statistics
   curl -s http://localhost:1848/metrics | grep -E "chainweb_system_(gc|memory)"
   ```

### Common Root Causes

#### 1. Memory Leaks
- **Symptoms:** Steadily increasing memory usage over time
- **Investigation:**
  ```bash
  # Track memory growth
  pmap -x $(pgrep chainweb-node) | tail -n 1

  # Check for heap growth
  curl -s http://localhost:1848/metrics | grep chainweb_system_memory_allocated_bytes
  ```

#### 2. Large Cache Sizes
- **Symptoms:** High but stable memory usage
- **Investigation:**
  ```bash
  # Check cache configurations
  curl -s http://localhost:1848/metrics | grep cache

  # Review cache hit rates
  curl -s http://localhost:1848/metrics | grep chainweb_database_cache_hit_rate
  ```

#### 3. High Transaction Volume
- **Symptoms:** Memory spikes during high activity
- **Investigation:**
  ```bash
  # Monitor mempool size
  curl -s http://localhost:1848/metrics | grep chainweb_mempool_current_size

  # Check transaction queue sizes
  curl -s http://localhost:1848/metrics | grep queue_depth
  ```

### Resolution Steps

#### Immediate Actions (Critical):
1. **Free up memory:**
   ```bash
   # Clear system caches
   echo 3 > /proc/sys/vm/drop_caches

   # Reduce mempool size temporarily
   curl -X POST http://localhost:1848/chainweb/0.0/mainnet01/mempool/cleanup

   # Stop non-essential services
   systemctl stop non-essential-service1
   ```

2. **Increase memory limits:**
   ```bash
   # Increase swap temporarily
   fallocate -l 4G /tmp/emergency_swap
   mkswap /tmp/emergency_swap
   swapon /tmp/emergency_swap
   ```

#### Medium-term fixes:
1. **Optimize memory usage:**
   ```bash
   # Reduce cache sizes
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_CACHE_SIZE=512M
   # Environment=CHAINWEB_MEMPOOL_SIZE=1000
   systemctl restart chainweb-node
   ```

2. **Tune GC settings:**
   ```bash
   # Optimize garbage collection
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_GHC_RTS_OPTS="+RTS -A64M -n4m -qg -H4G -RTS"
   systemctl restart chainweb-node
   ```

#### Long-term solutions:
1. **Memory upgrade:**
   ```bash
   # Plan for additional RAM based on usage patterns
   # Monitor peak memory requirements
   ```

2. **Architecture optimization:**
   ```bash
   # Implement memory-efficient data structures
   # Consider off-heap storage for large datasets
   ```

### Escalation Criteria
- Available memory <500MB
- OOM killer activation
- Swap usage >90%
- System becomes unresponsive

---

## Disk Exhaustion

### Alert: ChainwebDiskSpaceExhaustion / ChainwebHighDiskUsage

**Severity:** Critical / Warning
**Description:** Disk usage exceeding thresholds (>90% critical, >80% warning).

### Initial Investigation

1. **Check disk usage:**
   ```bash
   # Overall disk usage
   df -h

   # Specific mount points
   df -h /var/lib/chainweb /var/log/chainweb

   # Largest directories
   du -h --max-depth=2 /var/lib/chainweb | sort -hr | head -10
   ```

2. **Identify space usage:**
   ```bash
   # Database size
   du -sh /var/lib/chainweb/*/rocksDb/

   # Log file sizes
   du -sh /var/log/chainweb/*

   # Temporary files
   find /tmp /var/tmp -type f -size +100M -exec ls -lh {} \;
   ```

3. **Check disk I/O:**
   ```bash
   # I/O statistics
   iostat -x 1 5

   # Process I/O usage
   iotop -p $(pgrep chainweb-node) -n 5
   ```

### Resolution Steps

#### Immediate Actions (Critical):
1. **Free up space quickly:**
   ```bash
   # Clean old log files
   find /var/log/chainweb -name "*.log.*" -mtime +7 -delete
   journalctl --vacuum-time=3d

   # Clean temporary files
   find /tmp -type f -mtime +1 -delete

   # Remove old backups if safe
   find /var/lib/chainweb/backups -name "*.tar.gz" -mtime +30 -delete
   ```

2. **Emergency cleanup:**
   ```bash
   # Compress large log files
   gzip /var/log/chainweb/*.log

   # Move non-critical data
   mv /var/lib/chainweb/archived /mnt/external/
   ```

#### Medium-term fixes:
1. **Implement log rotation:**
   ```bash
   # Configure logrotate
   cat > /etc/logrotate.d/chainweb << EOF
   /var/log/chainweb/*.log {
       daily
       rotate 7
       compress
       delaycompress
       missingok
       notifempty
       copytruncate
   }
   EOF
   ```

2. **Database maintenance:**
   ```bash
   # Compact database
   systemctl stop chainweb-node
   cwtools db block-header-db compact --config /etc/chainweb/config.yaml
   systemctl start chainweb-node
   ```

#### Long-term solutions:
1. **Storage expansion:**
   ```bash
   # Plan for additional storage
   # Consider moving database to separate volume
   ```

2. **Automated cleanup:**
   ```bash
   # Set up automated maintenance scripts
   crontab -e
   # Add: 0 2 * * * /usr/local/bin/chainweb-cleanup.sh
   ```

### Escalation Criteria
- Available disk space <1GB
- Database corruption due to space exhaustion
- Node unable to start due to insufficient space

---

## GC Pauses

### Alert: ChainwebHighGCPauses / ChainwebGCFrequency

**Severity:** Critical / Info
**Description:** Garbage collection pauses affecting node responsiveness.

### Initial Investigation

1. **Check GC metrics:**
   ```bash
   # GC pause statistics
   curl -s http://localhost:1848/metrics | grep chainweb_system_gc_pause_duration_seconds

   # GC frequency
   curl -s http://localhost:1848/metrics | grep chainweb_system_gc_count_total

   # Memory allocation rate
   curl -s http://localhost:1848/metrics | grep chainweb_system_memory_allocated_bytes
   ```

2. **Analyze GC patterns:**
   ```bash
   # Monitor GC activity
   tail -f /var/log/chainweb/chainweb-node.log | grep -i gc

   # Check for memory pressure
   cat /proc/pressure/memory
   ```

### Resolution Steps

1. **Optimize GC settings:**
   ```bash
   # Tune GC parameters
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_GHC_RTS_OPTS="+RTS -A128M -n4m -qg -I0 -G1 -RTS"
   systemctl restart chainweb-node
   ```

2. **Reduce memory allocation:**
   ```bash
   # Lower cache sizes to reduce GC pressure
   systemctl edit chainweb-node
   # Add:
   # [Service]
   # Environment=CHAINWEB_CACHE_SIZE=256M
   systemctl restart chainweb-node
   ```

---

## Thread Count Issues

### Alert: ChainwebHighThreadCount

**Severity:** Warning
**Description:** Excessive thread count indicating potential thread leaks.

### Initial Investigation

1. **Check thread usage:**
   ```bash
   # Thread count
   cat /proc/$(pgrep chainweb-node)/status | grep Threads

   # Thread details
   ls /proc/$(pgrep chainweb-node)/task | wc -l

   # Thread stack traces
   kill -QUIT $(pgrep chainweb-node)  # Check logs for stack traces
   ```

### Resolution Steps

1. **Monitor for thread leaks:**
   ```bash
   # Track thread count over time
   while true; do
     echo "$(date): $(cat /proc/$(pgrep chainweb-node)/status | grep Threads)"
     sleep 300
   done
   ```

2. **Restart if necessary:**
   ```bash
   # If thread count continues growing
   systemctl restart chainweb-node
   ```

---

## Emergency Cleanup

### Emergency Resource Recovery Script

```bash
#!/bin/bash
# emergency-cleanup.sh

echo "Emergency cleanup initiated at $(date)"

# Stop non-essential services
systemctl stop prometheus grafana-server nginx

# Clear system caches
echo 3 > /proc/sys/vm/drop_caches

# Clean temporary files
find /tmp -type f -mtime +0 -delete 2>/dev/null

# Compress logs
gzip /var/log/chainweb/*.log 2>/dev/null

# Clean old logs
find /var/log -name "*.log.*" -mtime +3 -delete 2>/dev/null

# Clean journal
journalctl --vacuum-size=100M

# Free swap
swapoff -a && swapon -a

# Check results
echo "Available memory: $(free -h | grep ^Mem | awk '{print $7}')"
echo "Available disk: $(df -h / | tail -1 | awk '{print $4}')"

echo "Emergency cleanup completed at $(date)"
```

### Resource Monitoring Script

```bash
#!/bin/bash
# resource-monitor.sh

while true; do
  # CPU usage
  cpu=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}' | cut -d'%' -f1)

  # Memory usage
  mem=$(free | grep Mem | awk '{printf "%.1f", $3/$2 * 100.0}')

  # Disk usage
  disk=$(df / | tail -1 | awk '{print $5}' | cut -d'%' -f1)

  echo "$(date): CPU: ${cpu}%, Memory: ${mem}%, Disk: ${disk}%"

  # Alert if any threshold exceeded
  if (( $(echo "$cpu > 90" | bc -l) )) || (( $(echo "$mem > 85" | bc -l) )) || (( disk > 85 )); then
    echo "ALERT: Resource threshold exceeded!"
    # Trigger emergency procedures
  fi

  sleep 60
done
```

---

## Preventive Measures

1. **Regular maintenance:**
   - Schedule log rotation
   - Implement automated cleanup
   - Monitor resource trends

2. **Capacity planning:**
   - Track resource usage patterns
   - Plan for growth
   - Set up predictive alerting

3. **Configuration optimization:**
   - Tune memory settings
   - Optimize cache sizes
   - Configure appropriate limits

4. **Monitoring and alerting:**
   - Set up comprehensive resource monitoring
   - Implement escalation procedures
   - Regular health checks

---

## Related Documentation

- [Performance Issues Runbook](performance-issues.md)
- [Database Issues Runbook](database-issues.md)
- [Consensus Failures Runbook](consensus-failures.md)
- [System Administration Guide](https://github.com/kadena-io/chainweb-node/blob/master/docs/administration.md)
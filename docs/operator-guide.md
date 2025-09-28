# Chainweb Node Operator Guide

This comprehensive guide provides operators with everything needed to deploy, monitor, and maintain Chainweb nodes with Prometheus metrics and AlertManager integration.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Metrics Configuration](#metrics-configuration)
3. [Prometheus Setup](#prometheus-setup)
4. [AlertManager Configuration](#alertmanager-configuration)
5. [Grafana Dashboard Setup](#grafana-dashboard-setup)
6. [Alert Rules Deployment](#alert-rules-deployment)
7. [Monitoring Best Practices](#monitoring-best-practices)
8. [Troubleshooting](#troubleshooting)
9. [Maintenance Procedures](#maintenance-procedures)

## Prerequisites

### System Requirements

- **Operating System:** Linux (Ubuntu 20.04+ or CentOS 8+ recommended)
- **Hardware:**
  - CPU: 8+ cores (16+ recommended for mainnet)
  - Memory: 16GB+ RAM (32GB+ recommended)
  - Storage: 1TB+ SSD (fast NVMe recommended)
  - Network: 1Gbps+ connection, low latency
- **Software:**
  - Docker and Docker Compose OR native binary installation
  - systemd for service management

### Network Configuration

```bash
# Open required ports
sudo ufw allow 1789  # P2P networking
sudo ufw allow 1848  # Service API
sudo ufw allow 9090  # Prometheus (internal)
sudo ufw allow 9093  # AlertManager (internal)
sudo ufw allow 3000  # Grafana (internal/VPN only)

# Optional: Configure firewall rules for specific IPs
sudo ufw allow from 10.0.0.0/8 to any port 9090
sudo ufw allow from 192.168.0.0/16 to any port 3000
```

## Metrics Configuration

### Enabling Metrics in Chainweb Node

Add the following to your chainweb configuration file (`/etc/chainweb/config.yaml`):

```yaml
chainweb:
  # Basic node configuration
  node:
    chainwebVersion: mainnet01
    port: 1789
    interface: "0.0.0.0"

  # Service API configuration
  serviceApi:
    port: 1848
    interface: "0.0.0.0"

  # Enable metrics endpoint
  metrics:
    enabled: true
    endpoint: "/metrics"
    port: 1848  # Same as service API

  # Database configuration for metrics
  database:
    # Ensure adequate cache for metrics collection
    blockCache: 2147483648  # 2GB

  # P2P configuration
  p2p:
    peer:
      maxSessionCount: 20
      sessionTimeout: 300

  # Mining configuration (if applicable)
  mining:
    coordination:
      enabled: false

# Logging configuration
logging:
  level: info
  # Enable structured logging for better metrics parsing
  format: json
  destinations:
    - type: file
      path: /var/log/chainweb/chainweb-node.log
      maxSize: 100MB
      maxBackups: 10
```

### Environment Variables for Metrics

Set the following environment variables for optimal metrics collection:

```bash
# /etc/systemd/system/chainweb-node.service.d/metrics.conf
[Service]
Environment=CHAINWEB_METRICS_ENABLED=true
Environment=CHAINWEB_METRICS_INTERVAL=30
Environment=CHAINWEB_LOG_LEVEL=info
Environment=CHAINWEB_GHC_RTS_OPTS="+RTS -A32M -n2m -qg -H4G -T -RTS"
```

Reload and restart the service:

```bash
sudo systemctl daemon-reload
sudo systemctl restart chainweb-node
```

### Verify Metrics Endpoint

Test that metrics are available:

```bash
# Test metrics endpoint
curl -s http://localhost:1848/metrics | head -20

# Verify specific Chainweb metrics
curl -s http://localhost:1848/metrics | grep chainweb_ | head -10

# Check metrics format
curl -s http://localhost:1848/metrics | grep "# HELP" | head -5
```

## Prometheus Setup

### Prometheus Installation

#### Option 1: Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  prometheus:
    image: prom/prometheus:latest
    container_name: prometheus
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml
      - ./alerts:/etc/prometheus/alerts
      - prometheus_data:/prometheus
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--web.console.libraries=/etc/prometheus/console_libraries'
      - '--web.console.templates=/etc/prometheus/consoles'
      - '--storage.tsdb.retention.time=30d'
      - '--web.enable-lifecycle'
      - '--alertmanager.notification-queue-capacity=100'
    restart: unless-stopped

volumes:
  prometheus_data:
```

#### Option 2: Native Installation

```bash
# Download and install Prometheus
wget https://github.com/prometheus/prometheus/releases/download/v2.45.0/prometheus-2.45.0.linux-amd64.tar.gz
tar xvfz prometheus-*.tar.gz
sudo mv prometheus-*/prometheus /usr/local/bin/
sudo mv prometheus-*/promtool /usr/local/bin/

# Create system user and directories
sudo useradd --no-create-home --shell /bin/false prometheus
sudo mkdir /etc/prometheus /var/lib/prometheus
sudo chown prometheus:prometheus /etc/prometheus /var/lib/prometheus
```

### Prometheus Configuration

Create `/etc/prometheus/prometheus.yml`:

```yaml
global:
  scrape_interval: 30s
  evaluation_interval: 30s
  external_labels:
    monitor: 'chainweb-monitor'

rule_files:
  - "/etc/prometheus/alerts/consensus-alerts.yaml"
  - "/etc/prometheus/alerts/performance-alerts.yaml"
  - "/etc/prometheus/alerts/resource-alerts.yaml"

alerting:
  alertmanagers:
    - static_configs:
        - targets:
          - alertmanager:9093

scrape_configs:
  # Chainweb node metrics
  - job_name: 'chainweb-node'
    static_configs:
      - targets: ['localhost:1848']
    scrape_interval: 30s
    scrape_timeout: 10s
    metrics_path: /metrics

  # System metrics (node_exporter)
  - job_name: 'node-exporter'
    static_configs:
      - targets: ['localhost:9100']
    scrape_interval: 15s

  # Prometheus self-monitoring
  - job_name: 'prometheus'
    static_configs:
      - targets: ['localhost:9090']

  # Additional Chainweb nodes (if monitoring multiple)
  - job_name: 'chainweb-cluster'
    static_configs:
      - targets:
        - 'node1.example.com:1848'
        - 'node2.example.com:1848'
        - 'node3.example.com:1848'
    scrape_interval: 30s
```

### Node Exporter Installation

Install node_exporter for system metrics:

```bash
# Download and install node_exporter
wget https://github.com/prometheus/node_exporter/releases/download/v1.6.1/node_exporter-1.6.1.linux-amd64.tar.gz
tar xvfz node_exporter-*.tar.gz
sudo mv node_exporter-*/node_exporter /usr/local/bin/

# Create systemd service
sudo tee /etc/systemd/system/node_exporter.service > /dev/null <<EOF
[Unit]
Description=Node Exporter
Wants=network-online.target
After=network-online.target

[Service]
User=node_exporter
Group=node_exporter
Type=simple
ExecStart=/usr/local/bin/node_exporter

[Install]
WantedBy=multi-user.target
EOF

# Start service
sudo systemctl daemon-reload
sudo systemctl start node_exporter
sudo systemctl enable node_exporter
```

## AlertManager Configuration

### AlertManager Installation

```bash
# Download and install AlertManager
wget https://github.com/prometheus/alertmanager/releases/download/v0.25.0/alertmanager-0.25.0.linux-amd64.tar.gz
tar xvfz alertmanager-*.tar.gz
sudo mv alertmanager-*/alertmanager /usr/local/bin/
sudo mv alertmanager-*/amtool /usr/local/bin/

# Create directories
sudo mkdir /etc/alertmanager /var/lib/alertmanager
sudo chown alertmanager:alertmanager /etc/alertmanager /var/lib/alertmanager
```

### AlertManager Configuration

Create `/etc/alertmanager/alertmanager.yml`:

```yaml
global:
  smtp_smarthost: 'localhost:587'
  smtp_from: 'chainweb-alerts@example.com'
  slack_api_url: 'YOUR_SLACK_WEBHOOK_URL'

route:
  group_by: ['alertname', 'cluster', 'service']
  group_wait: 30s
  group_interval: 5m
  repeat_interval: 12h
  receiver: 'web.hook'
  routes:
    # Critical alerts go to PagerDuty and Slack
    - match:
        severity: critical
      receiver: 'critical-alerts'
      group_wait: 10s
      repeat_interval: 5m

    # Warning alerts go to Slack only
    - match:
        severity: warning
      receiver: 'warning-alerts'
      group_wait: 30s
      repeat_interval: 4h

    # Info alerts go to email
    - match:
        severity: info
      receiver: 'info-alerts'
      group_wait: 60s
      repeat_interval: 24h

receivers:
  - name: 'web.hook'
    webhook_configs:
      - url: 'http://localhost:5001/'

  - name: 'critical-alerts'
    pagerduty_configs:
      - service_key: 'YOUR_PAGERDUTY_SERVICE_KEY'
        description: 'Chainweb Critical Alert: {{ .GroupLabels.alertname }}'
        details:
          firing: '{{ range .Alerts.Firing }}{{ .Annotations.summary }}{{ end }}'
    slack_configs:
      - channel: '#chainweb-critical'
        title: 'CRITICAL: {{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'
        color: 'danger'

  - name: 'warning-alerts'
    slack_configs:
      - channel: '#chainweb-alerts'
        title: 'WARNING: {{ .GroupLabels.alertname }}'
        text: '{{ range .Alerts }}{{ .Annotations.description }}{{ end }}'
        color: 'warning'

  - name: 'info-alerts'
    email_configs:
      - to: 'chainweb-ops@example.com'
        subject: 'Chainweb Info Alert: {{ .GroupLabels.alertname }}'
        body: |
          {{ range .Alerts }}
          Alert: {{ .Annotations.summary }}
          Description: {{ .Annotations.description }}
          {{ end }}

inhibit_rules:
  - source_match:
      severity: 'critical'
    target_match:
      severity: 'warning'
    equal: ['alertname', 'cluster', 'service']
```

### AlertManager Systemd Service

Create `/etc/systemd/system/alertmanager.service`:

```ini
[Unit]
Description=AlertManager
Wants=network-online.target
After=network-online.target

[Service]
User=alertmanager
Group=alertmanager
Type=simple
ExecStart=/usr/local/bin/alertmanager \
  --config.file=/etc/alertmanager/alertmanager.yml \
  --storage.path=/var/lib/alertmanager/ \
  --web.external-url=http://localhost:9093
Restart=always

[Install]
WantedBy=multi-user.target
```

## Grafana Dashboard Setup

### Grafana Installation

```bash
# Add Grafana repository
wget -q -O - https://packages.grafana.com/gpg.key | sudo apt-key add -
echo "deb https://packages.grafana.com/oss/deb stable main" | sudo tee /etc/apt/sources.list.d/grafana.list

# Install Grafana
sudo apt update
sudo apt install grafana

# Start and enable Grafana
sudo systemctl start grafana-server
sudo systemctl enable grafana-server
```

### Grafana Data Source Configuration

1. **Access Grafana:** http://localhost:3000 (admin/admin)

2. **Add Prometheus Data Source:**
   - Go to Configuration → Data Sources
   - Add new Prometheus data source
   - URL: `http://localhost:9090`
   - Save & Test

### Dashboard JSON Templates

#### Node Overview Dashboard

Create a basic dashboard JSON that can be imported:

```json
{
  "dashboard": {
    "id": null,
    "title": "Chainweb Node Overview",
    "tags": ["chainweb"],
    "timezone": "browser",
    "panels": [
      {
        "id": 1,
        "title": "Cut Height",
        "type": "stat",
        "targets": [
          {
            "expr": "chainweb_blockchain_cut_height",
            "refId": "A"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 0, "y": 0}
      },
      {
        "id": 2,
        "title": "Block Heights by Chain",
        "type": "graph",
        "targets": [
          {
            "expr": "chainweb_blockchain_block_height",
            "refId": "A",
            "legendFormat": "Chain {{chain_id}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 12, "y": 0}
      }
    ]
  }
}
```

For complete dashboard templates, see the generated dashboards in the next section.

## Alert Rules Deployment

### Copy Alert Rules to Prometheus

```bash
# Copy alert rule files to Prometheus directory
sudo cp alerts/*.yaml /etc/prometheus/alerts/

# Set proper ownership
sudo chown prometheus:prometheus /etc/prometheus/alerts/*.yaml

# Validate alert rules
promtool check rules /etc/prometheus/alerts/*.yaml

# Reload Prometheus configuration
curl -X POST http://localhost:9090/-/reload
```

### Alert Rule Validation

```bash
# Check if rules are loaded
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].name'

# Test specific alert
curl -s 'http://localhost:9090/api/v1/query?query=ALERTS{alertname="ChainwebCutHeightStagnation"}'

# Check AlertManager targets
curl -s http://localhost:9093/api/v1/status | jq '.data.configYAML'
```

## Monitoring Best Practices

### Alert Tuning Guidelines

1. **Start Conservative:**
   ```yaml
   # Begin with higher thresholds, adjust based on normal operations
   expr: chainweb_p2p_request_duration_seconds > 2.0  # Start here
   # After observing normal patterns, tighten to:
   expr: chainweb_p2p_request_duration_seconds > 1.0  # Production value
   ```

2. **Use Appropriate Time Windows:**
   ```yaml
   # For volatile metrics, use longer evaluation periods
   expr: avg_over_time(chainweb_system_cpu_usage_percent[5m]) > 80
   for: 3m  # Prevent flapping
   ```

3. **Implement Alert Grouping:**
   ```yaml
   # Group related alerts to prevent spam
   route:
     group_by: ['alertname', 'chain_id', 'instance']
     group_wait: 30s
   ```

### Capacity Planning

Monitor these key metrics for capacity planning:

```yaml
# Resource utilization trends
- record: instance:cpu_utilization:rate5m
  expr: 100 - (avg(irate(node_cpu_seconds_total{mode="idle"}[5m])) * 100)

- record: instance:memory_utilization:ratio
  expr: (1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100

- record: instance:disk_utilization:ratio
  expr: 100 - ((node_filesystem_avail_bytes{mountpoint="/"} / node_filesystem_size_bytes{mountpoint="/"}) * 100)

# Chainweb-specific growth metrics
- record: chainweb:database_growth_rate:1d
  expr: rate(chainweb_system_database_size_bytes[1d])

- record: chainweb:transaction_rate:5m
  expr: rate(chainweb_mempool_transactions_inserted_total[5m])
```

### Alert Fatigue Prevention

1. **Use Alert Hierarchies:**
   ```yaml
   # Parent alert for system issues
   - alert: NodeDown
     expr: up{job="chainweb-node"} == 0

   # Child alerts only fire when parent is OK
   - alert: HighCPU
     expr: cpu_usage > 80 AND on(instance) up{job="chainweb-node"} == 1
   ```

2. **Implement Smart Grouping:**
   ```yaml
   route:
     group_by: ['alertname', 'cluster']
     group_wait: 30s
     group_interval: 5m
     repeat_interval: 4h
   ```

## Troubleshooting

### Common Issues and Solutions

#### Metrics Not Available

**Problem:** Metrics endpoint returns 404 or empty response

**Solutions:**
```bash
# Check if metrics are enabled
grep -i metrics /etc/chainweb/config.yaml

# Verify service is running
systemctl status chainweb-node

# Check logs for errors
journalctl -u chainweb-node -f | grep -i metrics

# Test endpoint manually
curl -v http://localhost:1848/metrics
```

#### Prometheus Not Scraping

**Problem:** Prometheus shows targets as down

**Solutions:**
```bash
# Check Prometheus targets page
curl -s http://localhost:9090/api/v1/targets | jq '.data.activeTargets[] | {job: .labels.job, health: .health}'

# Verify network connectivity
telnet localhost 1848

# Check Prometheus logs
journalctl -u prometheus -f

# Validate Prometheus configuration
promtool check config /etc/prometheus/prometheus.yml
```

#### Alerts Not Firing

**Problem:** Expected alerts are not triggering

**Solutions:**
```bash
# Check alert rules syntax
promtool check rules /etc/prometheus/alerts/*.yaml

# Query the alert expression manually
curl -s 'http://localhost:9090/api/v1/query?query=YOUR_ALERT_EXPRESSION'

# Check alert state in Prometheus
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].rules[] | select(.type=="alerting")'

# Verify AlertManager connectivity
curl -s http://localhost:9093/api/v1/status
```

#### High Cardinality Issues

**Problem:** Too many unique metric combinations causing performance issues

**Solutions:**
```bash
# Identify high cardinality metrics
curl -s http://localhost:9090/api/v1/label/__name__/values | jq '.data[]' | sort | uniq -c | sort -nr | head -10

# Check series count
curl -s 'http://localhost:9090/api/v1/query?query={__name__=~".+"}' | jq '.data.result | length'

# Implement metric relabeling in Prometheus config
metric_relabel_configs:
  - source_labels: [__name__]
    regex: 'high_cardinality_metric_.*'
    action: drop
```

### Performance Optimization

#### Prometheus Optimization

```yaml
# In prometheus.yml
global:
  # Reduce scrape frequency for less critical metrics
  scrape_interval: 60s
  evaluation_interval: 60s

# Increase retention based on disk space
command:
  - '--storage.tsdb.retention.time=90d'
  - '--storage.tsdb.retention.size=50GB'

# Optimize for SSD storage
  - '--storage.tsdb.wal-compression'
```

#### AlertManager Optimization

```yaml
# In alertmanager.yml
global:
  # Reduce notification frequency
  group_interval: 10m
  repeat_interval: 4h

route:
  # Batch alerts efficiently
  group_wait: 30s
  group_interval: 5m
```

## Maintenance Procedures

### Regular Maintenance Tasks

#### Daily
```bash
#!/bin/bash
# daily-maintenance.sh

# Check disk space
df -h | grep -E "(80%|90%|100%)" && echo "ALERT: High disk usage"

# Verify all services are running
systemctl is-active chainweb-node prometheus alertmanager grafana-server

# Check for any critical alerts
curl -s http://localhost:9093/api/v1/alerts | jq '.data[] | select(.labels.severity=="critical")'
```

#### Weekly
```bash
#!/bin/bash
# weekly-maintenance.sh

# Update system packages
sudo apt update && sudo apt upgrade -y

# Rotate logs if not automated
journalctl --vacuum-time=7d

# Check and compact database if needed
cwtools db block-header-db compact --config /etc/chainweb/config.yaml

# Review alert frequency and tune thresholds
curl -s http://localhost:9090/api/v1/rules | jq '.data.groups[].rules[] | select(.alerts) | .alerts[]' | sort | uniq -c
```

#### Monthly
```bash
#!/bin/bash
# monthly-maintenance.sh

# Backup configurations
tar -czf /backup/monitoring-config-$(date +%Y%m).tar.gz /etc/prometheus /etc/alertmanager /etc/grafana

# Review and update alert rules based on false positives
# Update Grafana dashboards
# Performance review and capacity planning
```

### Backup and Recovery

#### Configuration Backup
```bash
# Create backup script
#!/bin/bash
BACKUP_DIR="/backup/chainweb-monitoring"
DATE=$(date +%Y%m%d)

mkdir -p $BACKUP_DIR

# Backup configurations
tar -czf $BACKUP_DIR/prometheus-config-$DATE.tar.gz /etc/prometheus
tar -czf $BACKUP_DIR/alertmanager-config-$DATE.tar.gz /etc/alertmanager
tar -czf $BACKUP_DIR/grafana-config-$DATE.tar.gz /var/lib/grafana

# Backup alert rules
cp alerts/*.yaml $BACKUP_DIR/

# Cleanup old backups (keep 30 days)
find $BACKUP_DIR -name "*.tar.gz" -mtime +30 -delete
```

#### Recovery Procedures
```bash
# Restore from backup
sudo systemctl stop prometheus alertmanager grafana-server

# Restore configurations
sudo tar -xzf /backup/prometheus-config-YYYYMMDD.tar.gz -C /
sudo tar -xzf /backup/alertmanager-config-YYYYMMDD.tar.gz -C /
sudo tar -xzf /backup/grafana-config-YYYYMMDD.tar.gz -C /

# Set permissions
sudo chown -R prometheus:prometheus /etc/prometheus
sudo chown -R alertmanager:alertmanager /etc/alertmanager
sudo chown -R grafana:grafana /var/lib/grafana

# Restart services
sudo systemctl start prometheus alertmanager grafana-server
```

### Upgrade Procedures

#### Prometheus Upgrade
```bash
# Download new version
wget https://github.com/prometheus/prometheus/releases/download/v2.XX.0/prometheus-2.XX.0.linux-amd64.tar.gz

# Stop service
sudo systemctl stop prometheus

# Backup current binary
sudo cp /usr/local/bin/prometheus /usr/local/bin/prometheus.bak

# Install new version
tar xvfz prometheus-*.tar.gz
sudo cp prometheus-*/prometheus /usr/local/bin/

# Start service and verify
sudo systemctl start prometheus
curl -s http://localhost:9090/api/v1/status/config
```

### Security Considerations

#### Access Control
```bash
# Use authentication for Grafana
# Configure in /etc/grafana/grafana.ini
[auth]
disable_login_form = false

[security]
admin_user = admin
admin_password = your_secure_password

[users]
allow_sign_up = false
```

#### Network Security
```bash
# Use reverse proxy with SSL for external access
# Configure nginx/apache with SSL certificates
# Restrict access to monitoring ports
sudo ufw deny from any to any port 9090
sudo ufw allow from 10.0.0.0/8 to any port 9090
```

#### Credential Management
```yaml
# Use external secrets management
# Example with HashiCorp Vault integration
alertmanager_config:
  slack_api_url: "{{ vault_kv_get('secret/monitoring/slack').api_url }}"
  pagerduty_service_key: "{{ vault_kv_get('secret/monitoring/pagerduty').service_key }}"
```

## Next Steps

1. **Deploy the monitoring stack** following this guide
2. **Test alert rules** by simulating failure conditions
3. **Customize dashboards** based on your specific requirements
4. **Implement runbook procedures** from the runbooks directory
5. **Set up automated backup** and maintenance procedures
6. **Train team members** on monitoring and incident response

## Related Documentation

- [Alert Rules Documentation](../alerts/)
- [Runbooks](../runbooks/)
- [Grafana Dashboard Templates](../dashboards/)
- [Chainweb Configuration Reference](../configuration/)
- [Troubleshooting Guide](../troubleshooting/)

For additional support, contact the Chainweb development team or refer to the community documentation.
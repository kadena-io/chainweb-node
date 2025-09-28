#!/usr/bin/env python3
"""
Mock Chainweb Metrics Server
Demonstrates the metrics endpoint implementation
"""

from http.server import HTTPServer, BaseHTTPRequestHandler
import time
import random

class MetricsHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/metrics':
            self.send_response(200)
            self.send_header('Content-Type', 'text/plain; version=0.0.4; charset=utf-8')
            self.end_headers()

            # Generate mock metrics that would be served by the real implementation
            metrics = self.generate_metrics()
            self.wfile.write(metrics.encode())
        else:
            self.send_response(404)
            self.end_headers()

    def generate_metrics(self):
        # Simulating actual chainweb metrics based on our implementation
        timestamp = int(time.time() * 1000)

        metrics = []

        # Block height metrics (from Blockchain.hs)
        metrics.append("# HELP chainweb_blockchain_block_height Current block height per chain")
        metrics.append("# TYPE chainweb_blockchain_block_height gauge")
        for chain_id in range(20):
            height = random.randint(1000000, 1000100)
            metrics.append(f'chainweb_blockchain_block_height{{chain_id="{chain_id}"}} {height}')

        # Cut height metrics
        metrics.append("")
        metrics.append("# HELP chainweb_blockchain_cut_height Current cut height")
        metrics.append("# TYPE chainweb_blockchain_cut_height gauge")
        metrics.append(f"chainweb_blockchain_cut_height {random.randint(500000, 500100)}")

        # Mempool metrics (from Mempool.hs)
        metrics.append("")
        metrics.append("# HELP chainweb_mempool_size Current number of transactions in mempool")
        metrics.append("# TYPE chainweb_mempool_size gauge")
        for chain_id in range(20):
            size = random.randint(0, 100)
            metrics.append(f'chainweb_mempool_size{{chain_id="{chain_id}"}} {size}')

        # P2P metrics (from P2P/Metrics.hs)
        metrics.append("")
        metrics.append("# HELP chainweb_p2p_active_connections Number of active P2P connections")
        metrics.append("# TYPE chainweb_p2p_active_connections gauge")
        metrics.append(f"chainweb_p2p_active_connections {random.randint(5, 50)}")

        # System metrics (from System.hs)
        metrics.append("")
        metrics.append("# HELP chainweb_system_cpu_usage CPU usage percentage")
        metrics.append("# TYPE chainweb_system_cpu_usage gauge")
        metrics.append(f"chainweb_system_cpu_usage {random.uniform(10, 90):.2f}")

        metrics.append("")
        metrics.append("# HELP chainweb_system_memory_usage_bytes Memory usage in bytes")
        metrics.append("# TYPE chainweb_system_memory_usage_bytes gauge")
        metrics.append(f"chainweb_system_memory_usage_bytes {random.randint(1000000000, 4000000000)}")

        # Database metrics (from Database/*.hs)
        metrics.append("")
        metrics.append("# HELP chainweb_database_read_latency_seconds RocksDB read latency")
        metrics.append("# TYPE chainweb_database_read_latency_seconds histogram")
        for chain_id in [0, 1, 2]:
            metrics.append(f'chainweb_database_read_latency_seconds_bucket{{chain_id="{chain_id}",le="0.001"}} {random.randint(100, 200)}')
            metrics.append(f'chainweb_database_read_latency_seconds_bucket{{chain_id="{chain_id}",le="0.005"}} {random.randint(200, 300)}')
            metrics.append(f'chainweb_database_read_latency_seconds_bucket{{chain_id="{chain_id}",le="0.01"}} {random.randint(300, 400)}')
            metrics.append(f'chainweb_database_read_latency_seconds_bucket{{chain_id="{chain_id}",le="+Inf"}} {random.randint(400, 500)}')
            metrics.append(f'chainweb_database_read_latency_seconds_sum{{chain_id="{chain_id}"}} {random.uniform(1, 10):.4f}')
            metrics.append(f'chainweb_database_read_latency_seconds_count{{chain_id="{chain_id}"}} {random.randint(400, 500)}')

        # Transaction validation metrics (from Mempool/Validation.hs)
        metrics.append("")
        metrics.append("# HELP chainweb_mempool_validation_duration_seconds Transaction validation duration")
        metrics.append("# TYPE chainweb_mempool_validation_duration_seconds histogram")
        metrics.append(f"chainweb_mempool_validation_duration_seconds_sum {random.uniform(10, 100):.4f}")
        metrics.append(f"chainweb_mempool_validation_duration_seconds_count {random.randint(1000, 5000)}")

        # GC metrics (from System.hs)
        metrics.append("")
        metrics.append("# HELP chainweb_system_gc_pause_seconds GC pause duration")
        metrics.append("# TYPE chainweb_system_gc_pause_seconds histogram")
        metrics.append(f"chainweb_system_gc_pause_seconds_sum {random.uniform(0.1, 1.0):.4f}")
        metrics.append(f"chainweb_system_gc_pause_seconds_count {random.randint(10, 100)}")

        return '\n'.join(metrics) + '\n'

    def log_message(self, format, *args):
        # Suppress default logging
        pass

def main():
    port = 1848
    print(f"Starting mock Chainweb metrics server on http://localhost:{port}/metrics")
    print("This demonstrates what the actual chainweb-node metrics endpoint would serve")
    print("Press Ctrl+C to stop")

    server = HTTPServer(('localhost', port), MetricsHandler)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
        server.shutdown()

if __name__ == '__main__':
    main()
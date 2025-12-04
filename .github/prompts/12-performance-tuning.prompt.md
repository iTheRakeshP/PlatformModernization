````prompt
# Phase 9: Performance Tuning

> **Use Case**: Optimize batch job performance including memory profiling, throughput tuning, database optimization, and resource monitoring.

---

## Prompt

```
@workspace For the [JOB_NAME] modernization, implement performance optimizations:

1. Memory profiling and optimization:
   - Verify memory stays under 500MB budget
   - Profile with memory_profiler
   - Identify and fix memory leaks
2. Pipeline throughput tuning:
   - Optimize num_processors based on CPU cores
   - Tune queue sizes for backpressure
   - Balance reader speed vs processor capacity
3. Database optimization:
   - Tune batch_size for bulk inserts (5K-10K range)
   - Connection pool sizing (if using pool)
   - Index usage verification
4. I/O optimization:
   - Chunk size tuning (64KB default, may need adjustment)
   - Buffered vs unbuffered I/O analysis
   - SSD vs HDD considerations
5. Monitoring and metrics:
   - Add throughput metrics (records/sec)
   - Memory usage tracking
   - Queue depth monitoring

Follow patterns from 08-performance-tuning.instructions.md.
Generate profiling scripts and update configuration.
```

---

## Expected Files

```
python/[job_name]/
├── scripts/
│   ├── profile_memory.py      # Memory profiling script
│   ├── profile_throughput.py  # Throughput benchmarking
│   └── tune_parameters.py     # Parameter optimization
└── src/utility/
    └── performance_monitor.py # Runtime metrics collection
```

---

## Key Patterns

### Memory Profiler Script
```python
"""Memory profiling for batch pipeline."""
from memory_profiler import profile
import tracemalloc
from typing import Dict, Any

class MemoryProfiler:
    """Track memory usage during batch execution."""
    
    def __init__(self, budget_mb: int = 500):
        self._budget_bytes = budget_mb * 1024 * 1024
        self._snapshots: list = []
    
    def start(self) -> None:
        """Start memory tracking."""
        tracemalloc.start()
    
    def snapshot(self, label: str) -> Dict[str, Any]:
        """Take memory snapshot."""
        current, peak = tracemalloc.get_traced_memory()
        snapshot = {
            'label': label,
            'current_mb': current / (1024 * 1024),
            'peak_mb': peak / (1024 * 1024),
            'within_budget': peak < self._budget_bytes,
        }
        self._snapshots.append(snapshot)
        return snapshot
    
    def stop(self) -> Dict[str, Any]:
        """Stop tracking and return summary."""
        tracemalloc.stop()
        return {
            'snapshots': self._snapshots,
            'max_peak_mb': max(s['peak_mb'] for s in self._snapshots),
            'budget_mb': self._budget_bytes / (1024 * 1024),
        }
```

### Throughput Monitor
```python
import time
from dataclasses import dataclass, field
from typing import Dict, List
from threading import Lock

@dataclass
class ThroughputMetrics:
    """Track processing throughput."""
    window_size: int = 60  # seconds
    _records: List[tuple] = field(default_factory=list)
    _lock: Lock = field(default_factory=Lock)
    
    def record_batch(self, count: int) -> None:
        """Record a batch of processed records."""
        with self._lock:
            self._records.append((time.time(), count))
            # Prune old records
            cutoff = time.time() - self.window_size
            self._records = [(t, c) for t, c in self._records if t > cutoff]
    
    def get_throughput(self) -> float:
        """Get records per second over window."""
        with self._lock:
            if len(self._records) < 2:
                return 0.0
            total = sum(c for _, c in self._records)
            duration = self._records[-1][0] - self._records[0][0]
            return total / duration if duration > 0 else 0.0
```

### Queue Depth Monitor
```python
import logging
from typing import Dict
from multiprocessing import Queue as MPQueue
from queue import Queue

class QueueMonitor:
    """Monitor queue depths for backpressure detection."""
    
    def __init__(self, logger: logging.Logger):
        self._logger = logger
        self._queues: Dict[str, any] = {}
    
    def register(self, name: str, queue) -> None:
        """Register a queue for monitoring."""
        self._queues[name] = queue
    
    def get_depths(self) -> Dict[str, int]:
        """Get current depth of all queues."""
        depths = {}
        for name, q in self._queues.items():
            try:
                depths[name] = q.qsize()
            except NotImplementedError:
                depths[name] = -1  # macOS doesn't support qsize
        return depths
    
    def log_depths(self) -> None:
        """Log current queue depths."""
        depths = self.get_depths()
        self._logger.info(
            "Queue depths",
            extra={'queue_depths': depths}
        )
```

### Parameter Tuning Guide
```python
"""
Performance Tuning Parameters Reference

PIPELINE PARALLELISM
--------------------
num_processors: CPU cores - 1 (leave 1 for reader/main)
  - 4-core machine: 3 processors
  - 8-core machine: 7 processors
  - Monitor CPU usage, target 80-90%

num_writers: 2-4 (I/O bound, more doesn't help much)
  - Start with 2
  - Increase if DB becomes bottleneck
  - Each writer = 1 DB connection

QUEUE SIZES
-----------
raw_queue_max: 20-50 batches
  - Too small: reader idles waiting for processors
  - Too large: memory pressure
  - Monitor: if consistently full, add processors

processed_queue_max: 10-20 batches
  - Smaller than raw (processors faster than writers)
  - Monitor: if full, DB is bottleneck

I/O PARAMETERS
--------------
chunk_size: 64KB - 1MB
  - SSD: Can use larger (256KB-1MB)
  - HDD: Stick with 64KB
  - Network storage: 64KB optimal

batch_size (DB): 5,000 - 10,000 records
  - Too small: connection overhead
  - Too large: transaction log pressure
  - Monitor: INSERT time per batch
"""
```

---

## Configuration Updates

### performance_config.yaml
```yaml
performance:
  profiling:
    enabled: false  # Enable for troubleshooting
    memory_budget_mb: 500
    throughput_window_sec: 60
  
  monitoring:
    queue_depth_interval_sec: 10
    metrics_log_interval_sec: 30
  
  tuning:
    # Calculated based on available resources
    auto_tune: true
    cpu_target_percent: 85
    memory_target_percent: 80
```

### Tuning Recommendations by File Size

| File Size | Processors | Writers | Raw Queue | Batch Size |
|-----------|------------|---------|-----------|------------|
| < 1GB     | 2          | 1       | 10        | 5,000      |
| 1-10GB    | 4          | 2       | 20        | 5,000      |
| 10-50GB   | 6          | 2       | 30        | 7,500      |
| 50-100GB  | 8          | 3       | 40        | 10,000     |
| > 100GB   | CPU-1      | 4       | 50        | 10,000     |

---

## Benchmarking Commands

```bash
# Profile memory usage
python -m memory_profiler scripts/profile_memory.py

# Run throughput benchmark
python scripts/profile_throughput.py --records 100000

# Monitor live performance
python -m src.batch.app --config config/ --profile
```

---

## Prerequisites
- All previous phases complete
- Sample data for benchmarking
- Production-like environment for accurate profiling

---

## Next Steps
- Review metrics and adjust configuration
- Run load tests with production-sized files
- Document optimal settings for production deployment

````

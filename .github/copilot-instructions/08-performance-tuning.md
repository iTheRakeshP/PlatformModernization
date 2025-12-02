# Performance Tuning

## Target Performance

| Metric | Target | Measure |
|--------|--------|---------|
| **Throughput** | 10,000+ records/second | `total_records / duration_seconds` |
| **Memory** | < 500MB | Process RSS (regardless of file size) |
| **CPU** | < 80% average | Per-core utilization |
| **DB Latency** | < 100ms per batch | Batch insert duration |

## Quick Tuning Guide

| Parameter | Default | When to Increase | When to Decrease |
|-----------|---------|------------------|------------------|
| `NUM_PROCESSORS` | 4 | CPU < 60% | CPU > 90% |
| `NUM_WRITERS` | 2 | Queue growing, DB idle | DB connection errors |
| `RAW_QUEUE_MAX` | 20 | Readers waiting | High memory usage |
| `PROCESSED_QUEUE_MAX` | 10 | Processors waiting | High memory usage |
| `BATCH_SIZE` | 5,000 | DB fast, need throughput | Lock contention, OOM |
| `CHUNK_SIZE` | 64KB | Rarely change | Rarely change |

## Hybrid Pipeline Optimization

### Why This Architecture is Fast

```
┌─────────────────────────────────────────────────────────────────┐
│                    PERFORMANCE CHARACTERISTICS                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  FILE I/O (Sequential)                                          │
│  ├── 64KB chunks = 100x fewer syscalls vs line-by-line         │
│  └── Sequential reads = disk cache friendly                     │
│                                                                  │
│  CPU WORK (multiprocessing)                                     │
│  ├── Bypasses Python GIL completely                             │
│  ├── True parallel execution on multiple cores                  │
│  └── Scale: 1 processor per CPU core                           │
│                                                                  │
│  DB I/O (threading)                                             │
│  ├── GIL released during network I/O                           │
│  ├── Lighter weight than processes                              │
│  ├── Shared memory = easy metrics aggregation                  │
│  └── Scale: 2-4 writers typically sufficient                   │
│                                                                  │
│  BATCHING                                                        │
│  ├── 5,000 records per DB batch = fewer round trips            │
│  ├── executemany() = single network call                       │
│  └── Commit per batch = transaction control                    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Tuning by Bottleneck

**Symptom: Low CPU, Queue Growing**
```python
# Problem: Writers can't keep up with processors
# Solution: Add more writer threads
NUM_WRITERS = 4  # Increase from 2

# Or increase batch size for fewer round trips
BATCH_SIZE = 10000  # Increase from 5000
```

**Symptom: High CPU, Queue Empty**
```python
# Problem: Readers/processors can't feed writers fast enough
# Solution: Add more processors (if I/O bound on read)
NUM_PROCESSORS = 8  # Increase from 4

# Or switch to multi-reader mode with pre-split files
reading_mode: "multi"
```

**Symptom: High Memory Usage**
```python
# Problem: Queues too large
# Solution: Reduce queue sizes
RAW_QUEUE_MAX = 10       # Reduce from 20
PROCESSED_QUEUE_MAX = 5  # Reduce from 10

# Or reduce batch size
BATCH_SIZE = 2000  # Reduce from 5000
```

**Symptom: DB Lock Contention**
```python
# Problem: Too many concurrent writers
# Solution: Reduce writers, increase batch size
NUM_WRITERS = 1
BATCH_SIZE = 10000

# Or use table partitioning
```

## Memory Budget

| Component | Per-Instance | Count | Total |
|-----------|--------------|-------|-------|
| Read buffer | 64KB | 1 | 64KB |
| Raw batch | ~25MB | 20 max | 500MB max |
| Processed batch | ~25MB | 10 max | 250MB max |
| DB connection | ~5MB | 2 | 10MB |
| Python overhead | ~50MB | 1 | 50MB |
| **Total** | - | - | **< 900MB** |

## Monitoring for Tuning

```python
import time
from queue import Queue
from threading import Thread

class PerformanceMonitor:
    """Monitor pipeline performance for tuning decisions."""
    
    def __init__(self, raw_queue, processed_queue, interval: float = 5.0):
        self.raw_queue = raw_queue
        self.processed_queue = processed_queue
        self.interval = interval
        self._running = False
    
    def start(self):
        self._running = True
        Thread(target=self._monitor_loop, daemon=True).start()
    
    def _monitor_loop(self):
        while self._running:
            raw_depth = self.raw_queue.qsize()
            proc_depth = self.processed_queue.qsize()
            
            # Log for analysis
            print(f"Queue depths: raw={raw_depth}, processed={proc_depth}")
            
            # Alert conditions
            if raw_depth > RAW_QUEUE_MAX * 0.8:
                print("WARNING: Raw queue near capacity - processors may be slow")
            if proc_depth > PROCESSED_QUEUE_MAX * 0.8:
                print("WARNING: Processed queue near capacity - writers may be slow")
            if raw_depth == 0 and proc_depth == 0:
                print("INFO: Queues empty - pipeline may be I/O bound")
            
            time.sleep(self.interval)
```

## Database Optimization

```python
# GOOD: Batch inserts with executemany
sql = "INSERT INTO STG_BATCH (RUN_ID, ACCT_ID, ...) VALUES (?, ?, ...)"
cursor.executemany(sql, batch_of_5000_records)  # Single network call

# BAD: Individual inserts
for record in records:
    cursor.execute(sql, record)  # 5000 network calls!
```

### DB2-Specific Optimizations

```sql
-- 1. Staging table with minimal indexes (insert-optimized)
CREATE TABLE STG_BATCH (
    STG_ID BIGINT GENERATED ALWAYS AS IDENTITY,
    RUN_ID VARCHAR(50),
    -- data columns...
    -- NO secondary indexes during load!
);

-- 2. Use APPEND mode for bulk inserts
-- (configure in connection or ibm_db settings)

-- 3. After load completes, MERGE to final table
MERGE INTO FINAL_BATCH AS T
USING (SELECT * FROM STG_BATCH WHERE RUN_ID = ?) AS S
ON (T.ACCT_ID = S.ACCT_ID)
WHEN MATCHED THEN UPDATE SET ...
WHEN NOT MATCHED THEN INSERT ...;
```

## Configuration Template

```yaml
# config/base_config.yaml
pipeline:
  reading_mode: "single"  # "single" or "multi"
  
  # Parallelism settings
  num_processors: 4       # 1 per CPU core
  num_writers: 2          # Start with 2, increase if DB idle
  
  # Queue settings
  raw_queue_max: 20       # Reader → Processors
  processed_queue_max: 10 # Processors → Writers
  
  # Batch settings
  batch_size: 5000        # Records per DB batch
  chunk_size: 65536       # 64KB file read buffer

db:
  # Connection per writer (not pooled)
  connection_timeout: 30
  query_timeout: 60
  
  # Retry settings
  max_retries: 3
  retry_backoff: 2.0      # Exponential: 1s, 2s, 4s
```

## Benchmarking

```python
import time
from dataclasses import dataclass

@dataclass
class BenchmarkResult:
    total_records: int
    duration_seconds: float
    records_per_second: float
    peak_memory_mb: float
    
def benchmark_pipeline(config: dict, input_file: str) -> BenchmarkResult:
    import psutil
    
    start_time = time.time()
    start_memory = psutil.Process().memory_info().rss / 1024 / 1024
    
    # Run pipeline
    orchestrator = PipelineOrchestrator(config)
    result = orchestrator.process_file(input_file)
    
    end_time = time.time()
    peak_memory = psutil.Process().memory_info().rss / 1024 / 1024
    
    duration = end_time - start_time
    total_records = result['total_written']
    
    return BenchmarkResult(
        total_records=total_records,
        duration_seconds=duration,
        records_per_second=total_records / duration,
        peak_memory_mb=peak_memory - start_memory
    )
```

## Reference

For comprehensive performance analysis and advanced tuning, see Section 14 of the main guide.
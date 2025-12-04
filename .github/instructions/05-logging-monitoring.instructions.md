# Logging & Monitoring

## Concept

A common logger class providing unified logging across all batch components. Supports all log levels (DEBUG, INFO, WARNING, ERROR, CRITICAL) with structured JSON output for centralized aggregation (Splunk/ELK). Automatically includes context like RUN_ID, job name, and correlation IDs in every log entry.

**Hybrid Pipeline Context**: With processors (multiprocessing) and writer threads (threading), each worker must include `worker_type` and `worker_id` in every log entry for traceability:

```
┌─────────────────────────────────────────────────────────────────┐
│                     LOGGING ARCHITECTURE                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Orchestrator ──► BatchLogger (run_id, job_name)                │
│       │                                                          │
│       ├── Processor 1 ──► WorkerLogger (worker_type=processor,  │
│       │   (Process)                     worker_id=1,            │
│       │                                 process_name=...)       │
│       │                                                          │
│       ├── Processor 2 ──► WorkerLogger (worker_type=processor,  │
│       │   (Process)                     worker_id=2,            │
│       │                                 process_name=...)       │
│       │                                                          │
│       ├── Writer 1 ──► WorkerLogger (worker_type=writer,        │
│       │   (Thread)                   worker_id=1,               │
│       │                              thread_name=DbWriter-1)    │
│       │                                                          │
│       └── Writer 2 ──► WorkerLogger (worker_type=writer,        │
│           (Thread)                   worker_id=2,               │
│                                      thread_name=DbWriter-2)    │
│                                                                  │
│  All logs ──► JSON ──► Splunk/ELK (filter by worker_id/type)    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## When to Use

✅ **Use when:**
- Building any batch processing component
- Need consistent log format across all modules
- Integrating with centralized log aggregation (Splunk, ELK, Datadog)
- Require audit trails with RUN_ID traceability
- Debugging production issues with structured data

❌ **Not needed when:**
- Simple scripts with console-only output
- Development/testing with basic print statements

## Complete Implementation

### 1. Common Logger Class (All Log Levels)

```python
import logging
import json
import sys
import traceback
from datetime import datetime, timezone
from typing import Optional, Dict, Any
from pathlib import Path
from functools import wraps


class BatchLogger:
    """
    Common logger class for batch processing.
    Provides unified logging with DEBUG, INFO, WARNING, ERROR, CRITICAL levels.
    Outputs structured JSON for log aggregation systems.
    """
    
    # Log level mapping
    LEVELS = {
        'DEBUG': logging.DEBUG,
        'INFO': logging.INFO,
        'WARNING': logging.WARNING,
        'ERROR': logging.ERROR,
        'CRITICAL': logging.CRITICAL
    }
    
    def __init__(self,
                 name: str,
                 level: str = 'INFO',
                 log_file: Optional[str] = None,
                 json_format: bool = True,
                 console_output: bool = True):
        """
        Initialize the common logger.
        
        Args:
            name: Logger name (usually module or job name)
            level: Minimum log level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
            log_file: Optional file path for log output
            json_format: Use JSON format (True) or text format (False)
            console_output: Also output to console
        """
        self._logger = logging.getLogger(name)
        self._logger.setLevel(self.LEVELS.get(level.upper(), logging.INFO))
        self._logger.handlers = []  # Clear existing handlers
        
        self._json_format = json_format
        self._context: Dict[str, Any] = {}
        
        # Create formatter
        if json_format:
            formatter = JSONFormatter()
        else:
            formatter = logging.Formatter(
                '%(asctime)s | %(levelname)-8s | %(name)s | %(message)s',
                datefmt='%Y-%m-%d %H:%M:%S'
            )
        
        # Console handler
        if console_output:
            console_handler = logging.StreamHandler(sys.stdout)
            console_handler.setFormatter(formatter)
            self._logger.addHandler(console_handler)
        
        # File handler
        if log_file:
            log_path = Path(log_file)
            log_path.parent.mkdir(parents=True, exist_ok=True)
            file_handler = logging.FileHandler(log_file, encoding='utf-8')
            file_handler.setFormatter(formatter)
            self._logger.addHandler(file_handler)
    
    def set_context(self, **kwargs) -> 'BatchLogger':
        """
        Set persistent context that will be included in all log entries.
        
        Example:
            logger.set_context(run_id='20251201_120000', job_name='CUSTOMER_LOAD')
        """
        self._context.update(kwargs)
        return self
    
    def clear_context(self) -> 'BatchLogger':
        """Clear all context."""
        self._context.clear()
        return self
    
    def _log(self, level: int, msg: str, **kwargs) -> None:
        """Internal log method with context merging."""
        extra = {**self._context, **kwargs}
        self._logger.log(level, msg, extra=extra)
    
    # ========== All Log Levels ==========
    
    def debug(self, msg: str, **kwargs) -> None:
        """Log DEBUG level - detailed diagnostic information."""
        self._log(logging.DEBUG, msg, **kwargs)
    
    def info(self, msg: str, **kwargs) -> None:
        """Log INFO level - general operational messages."""
        self._log(logging.INFO, msg, **kwargs)
    
    def warning(self, msg: str, **kwargs) -> None:
        """Log WARNING level - unexpected but handled situations."""
        self._log(logging.WARNING, msg, **kwargs)
    
    def error(self, msg: str, exc_info: bool = False, **kwargs) -> None:
        """
        Log ERROR level - error conditions that need attention.
        
        Args:
            msg: Error message
            exc_info: Include exception traceback if True
            **kwargs: Additional context
        """
        if exc_info:
            kwargs['exception'] = traceback.format_exc()
        self._log(logging.ERROR, msg, **kwargs)
    
    def critical(self, msg: str, exc_info: bool = False, **kwargs) -> None:
        """
        Log CRITICAL level - severe errors requiring immediate attention.
        
        Args:
            msg: Critical error message
            exc_info: Include exception traceback if True
            **kwargs: Additional context
        """
        if exc_info:
            kwargs['exception'] = traceback.format_exc()
        self._log(logging.CRITICAL, msg, **kwargs)
    
    def exception(self, msg: str, **kwargs) -> None:
        """Log ERROR level with automatic exception info capture."""
        kwargs['exception'] = traceback.format_exc()
        self._log(logging.ERROR, msg, **kwargs)
    
    # ========== Batch-Specific Convenience Methods ==========
    
    def log_batch_start(self, batch_size: int, batch_num: int) -> None:
        """Log start of batch processing."""
        self.info(
            f"Processing batch {batch_num}",
            batch_num=batch_num,
            batch_size=batch_size,
            event_type='batch_start'
        )
    
    def log_batch_complete(self, batch_num: int, records_processed: int, 
                           duration_ms: float) -> None:
        """Log completion of batch processing."""
        self.info(
            f"Batch {batch_num} completed: {records_processed} records in {duration_ms:.2f}ms",
            batch_num=batch_num,
            records_processed=records_processed,
            duration_ms=duration_ms,
            event_type='batch_complete'
        )
    
    def log_job_start(self, job_name: str, run_id: str) -> None:
        """Log job start with context setup."""
        self.set_context(run_id=run_id, job_name=job_name)
        self.info(
            f"Job started: {job_name}",
            event_type='job_start',
            start_time=datetime.now(timezone.utc).isoformat()
        )
    
    def log_job_complete(self, status: str, total_records: int, 
                         duration_seconds: float) -> None:
        """Log job completion."""
        level = logging.INFO if status == 'SUCCESS' else logging.ERROR
        self._log(
            level,
            f"Job completed with status: {status}",
            status=status,
            total_records=total_records,
            duration_seconds=duration_seconds,
            event_type='job_complete',
            end_time=datetime.now(timezone.utc).isoformat()
        )
    
    def log_validation_error(self, line_no: int, field: str, 
                             value: Any, error: str) -> None:
        """Log data validation error."""
        self.warning(
            f"Validation error at line {line_no}: {field} - {error}",
            line_no=line_no,
            field=field,
            invalid_value=str(value)[:100],  # Truncate long values
            validation_error=error,
            event_type='validation_error'
        )
    
    def log_db_operation(self, operation: str, table: str, 
                         row_count: int, duration_ms: float) -> None:
        """Log database operation."""
        self.debug(
            f"DB {operation} on {table}: {row_count} rows in {duration_ms:.2f}ms",
            db_operation=operation,
            table_name=table,
            row_count=row_count,
            duration_ms=duration_ms,
            event_type='db_operation'
        )


class JSONFormatter(logging.Formatter):
    """
    JSON formatter for structured logging.
    Compatible with Splunk, ELK, Datadog, and other log aggregators.
    """
    
    def format(self, record: logging.LogRecord) -> str:
        log_data = {
            'timestamp': datetime.now(timezone.utc).isoformat(),
            'level': record.levelname,
            'logger': record.name,
            'message': record.getMessage(),
            'module': record.module,
            'function': record.funcName,
            'line': record.lineno
        }
        
        # Add all extra fields from context
        for key, value in record.__dict__.items():
            if key not in ('name', 'msg', 'args', 'created', 'filename', 
                          'funcName', 'levelname', 'levelno', 'lineno',
                          'module', 'msecs', 'pathname', 'process',
                          'processName', 'relativeCreated', 'stack_info',
                          'thread', 'threadName', 'exc_info', 'exc_text',
                          'message', 'taskName'):
                log_data[key] = value
        
        # Handle exception info
        if record.exc_info:
            log_data['exception'] = self.formatException(record.exc_info)
        
        return json.dumps(log_data, default=str)
```

### 2. Logger Factory (Singleton Pattern)

```python
from threading import Lock

class LoggerFactory:
    """
    Factory for creating and managing logger instances.
    Ensures consistent configuration across the application.
    """
    
    _instances: Dict[str, BatchLogger] = {}
    _lock = Lock()
    _default_config = {
        'level': 'INFO',
        'json_format': True,
        'console_output': True,
        'log_file': None
    }
    
    @classmethod
    def configure(cls, **kwargs) -> None:
        """
        Configure default settings for all loggers.
        
        Example:
            LoggerFactory.configure(
                level='DEBUG',
                log_file='logs/batch.log',
                json_format=True
            )
        """
        cls._default_config.update(kwargs)
    
    @classmethod
    def get_logger(cls, name: str, **kwargs) -> BatchLogger:
        """
        Get or create a logger instance.
        
        Args:
            name: Logger name (module or component name)
            **kwargs: Override default configuration
        
        Returns:
            BatchLogger instance
        """
        with cls._lock:
            if name not in cls._instances:
                config = {**cls._default_config, **kwargs}
                cls._instances[name] = BatchLogger(name, **config)
            return cls._instances[name]
    
    @classmethod
    def get_job_logger(cls, job_name: str, run_id: str) -> BatchLogger:
        """
        Get a logger pre-configured with job context.
        
        Args:
            job_name: Name of the batch job
            run_id: Unique run identifier
        
        Returns:
            BatchLogger with run_id and job_name in context
        """
        logger = cls.get_logger(job_name)
        logger.set_context(run_id=run_id, job_name=job_name)
        return logger
```

### 3. Logging Decorator for Functions

```python
def log_execution(logger: BatchLogger = None, level: str = 'DEBUG'):
    """
    Decorator to automatically log function entry, exit, and exceptions.
    
    Args:
        logger: BatchLogger instance (uses function module logger if None)
        level: Log level for entry/exit (DEBUG, INFO)
    
    Example:
        @log_execution(logger, level='INFO')
        def process_batch(records):
            ...
    """
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            nonlocal logger
            if logger is None:
                logger = LoggerFactory.get_logger(func.__module__)
            
            func_name = func.__name__
            log_method = getattr(logger, level.lower())
            
            # Log entry
            log_method(
                f"Entering {func_name}",
                function=func_name,
                event_type='function_entry'
            )
            
            start_time = datetime.now()
            
            try:
                result = func(*args, **kwargs)
                
                # Log exit
                duration_ms = (datetime.now() - start_time).total_seconds() * 1000
                log_method(
                    f"Exiting {func_name}",
                    function=func_name,
                    duration_ms=duration_ms,
                    event_type='function_exit'
                )
                
                return result
                
            except Exception as e:
                # Log exception
                duration_ms = (datetime.now() - start_time).total_seconds() * 1000
                logger.error(
                    f"Exception in {func_name}: {str(e)}",
                    function=func_name,
                    duration_ms=duration_ms,
                    event_type='function_error',
                    exc_info=True
                )
                raise
        
        return wrapper
    return decorator
```

### 4. Worker-Specific Logging (Hybrid Pipeline Pattern)

When using the hybrid producer-consumer pipeline:
- **Processors** run in separate **processes** (multiprocessing)
- **Writers** run in separate **threads** (threading)

Each worker must include its identifier in every log entry for traceability:

```python
from multiprocessing import current_process
from threading import current_thread


class WorkerLogger:
    """
    Logger wrapper that automatically includes worker context.
    
    Works for both:
    - Processor workers (multiprocessing) - includes process_name
    - Writer threads (threading) - includes thread_name
    """
    
    def __init__(self, base_logger: BatchLogger, worker_type: str, worker_id: int):
        """
        Args:
            base_logger: Underlying BatchLogger instance
            worker_type: 'reader', 'processor', or 'writer'
            worker_id: Unique worker identifier
        """
        self._logger = base_logger
        self._worker_type = worker_type
        self._worker_id = worker_id
        
        # Determine if running in process or thread context
        if worker_type in ('reader', 'processor'):
            # Multiprocessing context
            self._context_name = current_process().name
            self._context_type = 'process'
        else:
            # Threading context (writers)
            self._context_name = current_thread().name
            self._context_type = 'thread'
        
        # Set persistent worker context
        self._logger.set_context(
            worker_type=worker_type,
            worker_id=worker_id,
            **{f'{self._context_type}_name': self._context_name}
        )
    
    def info(self, msg: str, **kwargs) -> None:
        self._logger.info(msg, **kwargs)
    
    def debug(self, msg: str, **kwargs) -> None:
        self._logger.debug(msg, **kwargs)
    
    def warning(self, msg: str, **kwargs) -> None:
        self._logger.warning(msg, **kwargs)
    
    def error(self, msg: str, exc_info: bool = False, **kwargs) -> None:
        self._logger.error(msg, exc_info=exc_info, **kwargs)
    
    def log_chunk_read(self, bytes_read: int, records_parsed: int) -> None:
        """Log chunk read by reader worker (multiprocessing)."""
        self.debug(
            f"Chunk read: {bytes_read} bytes, {records_parsed} records",
            bytes_read=bytes_read,
            records_parsed=records_parsed,
            event_type='chunk_read'
        )
    
    def log_batch_queued(self, batch_num: int, record_count: int) -> None:
        """Log batch placed in queue by processor (multiprocessing)."""
        self.debug(
            f"Batch {batch_num} queued with {record_count} records",
            batch_num=batch_num,
            record_count=record_count,
            event_type='batch_queued'
        )
    
    def log_batch_written(self, batch_num: int, record_count: int, 
                          duration_ms: float) -> None:
        """Log batch written to DB by writer thread (threading)."""
        self.info(
            f"Batch {batch_num} written: {record_count} records in {duration_ms:.2f}ms",
            batch_num=batch_num,
            record_count=record_count,
            duration_ms=duration_ms,
            event_type='batch_written'
        )
    
    def log_worker_complete(self, total_records: int, total_batches: int) -> None:
        """Log worker completion summary."""
        self.info(
            f"Worker complete: {total_records} records in {total_batches} batches",
            total_records=total_records,
            total_batches=total_batches,
            event_type='worker_complete'
        )


# Usage in ReaderProcessor:
class ReaderProcessor:
    def __init__(self, worker_id: int, ...):
        self.worker_id = worker_id
        base_logger = LoggerFactory.get_logger(f'reader_{worker_id}')
        self.logger = WorkerLogger(base_logger, 'reader', worker_id)
    
    def run(self):
        self.logger.info("Reader worker starting", 
                         partition_start=self.partition.start,
                         partition_end=self.partition.end)
        
        for chunk in self.read_chunks():
            records = self.parse(chunk)
            self.logger.log_chunk_read(len(chunk), len(records))
            # ... process and queue
        
        self.logger.log_worker_complete(self.total_records, self.total_batches)


# Usage in DbWriter (thread):
class DbWriter(threading.Thread):
    def __init__(self, writer_id: int, ...):
        super().__init__(name=f"DbWriter-{writer_id}", daemon=True)
        self.writer_id = writer_id
        base_logger = LoggerFactory.get_logger(f'writer_{writer_id}')
        self.logger = WorkerLogger(base_logger, 'writer', writer_id)
```

**Sample Worker Log Output (Writer Thread):**
```json
{
  "timestamp": "2025-12-01T14:32:15.123456Z",
  "level": "INFO",
  "message": "Batch 42 written: 5000 records in 234.56ms",
  "run_id": "20251201_140000",
  "worker_type": "writer",
  "worker_id": 2,
  "thread_name": "DbWriter-2",
  "batch_num": 42,
  "record_count": 5000,
  "duration_ms": 234.56,
  "event_type": "batch_written"
}
```

**Sample Worker Log Output (Processor Process):**
```json
{
  "timestamp": "2025-12-01T14:32:14.987654Z",
  "level": "DEBUG",
  "message": "Batch 41 queued with 5000 records",
  "run_id": "20251201_140000",
  "worker_type": "processor",
  "worker_id": 1,
  "process_name": "Processor-1",
  "batch_num": 41,
  "record_count": 5000,
  "event_type": "batch_queued"
}
```

### 5. Queue Depth Monitoring

Monitor the bounded queue to detect backpressure issues:

```python
import time
from queue import Queue  # threading.Queue for writer threads
from threading import Thread


class QueueMonitor:
    """
    Monitors queue depth and logs warnings when backpressure builds.
    Runs as a background thread in the orchestrator.
    """
    
    def __init__(self, queue: Queue, max_size: int, 
                 logger: BatchLogger, interval: float = 5.0):
        self.queue = queue
        self.max_size = max_size
        self.logger = logger
        self.interval = interval
        self._running = False
        self._thread = None
    
    def start(self) -> None:
        """Start monitoring in background thread."""
        self._running = True
        self._thread = Thread(target=self._monitor_loop, daemon=True)
        self._thread.start()
    
    def stop(self) -> None:
        """Stop monitoring."""
        self._running = False
        if self._thread:
            self._thread.join(timeout=2.0)
    
    def _monitor_loop(self) -> None:
        """Main monitoring loop."""
        while self._running:
            try:
                depth = self.queue.qsize()
                utilization = (depth / self.max_size) * 100
                
                self.logger.debug(
                    f"Queue depth: {depth}/{self.max_size} ({utilization:.1f}%)",
                    queue_depth=depth,
                    queue_max=self.max_size,
                    queue_utilization=utilization,
                    event_type='queue_status'
                )
                
                # Warn if queue is filling up (readers waiting)
                if utilization > 80:
                    self.logger.warning(
                        f"Queue backpressure: {utilization:.1f}% full - writers may be slow",
                        queue_depth=depth,
                        queue_utilization=utilization,
                        event_type='backpressure_warning'
                    )
                
                time.sleep(self.interval)
                
            except Exception as e:
                self.logger.error(f"Queue monitor error: {e}", exc_info=True)


# Usage in PipelineOrchestrator:
queue_monitor = QueueMonitor(batch_queue, max_size=10, logger=orchestrator_logger)
queue_monitor.start()
# ... run pipeline ...
queue_monitor.stop()
```

### 6. Complete Usage Example

```python
from datetime import datetime

# 1. Configure factory (once at application startup)
LoggerFactory.configure(
    level='DEBUG',
    log_file='logs/batch_20251201.log',
    json_format=True,
    console_output=True
)

# 2. Get logger for a module
logger = LoggerFactory.get_logger('io_layer.ebcdic_reader')

# 3. Or get job-specific logger with context
run_id = datetime.now().strftime('%Y%m%d_%H%M%S')
job_logger = LoggerFactory.get_job_logger('CUSTOMER_LOAD', run_id)

# 4. Use all log levels
job_logger.debug("Reading configuration", config_file='config/prod.yaml')
job_logger.info("Starting file processing", file_path='/data/input.dat')
job_logger.warning("Skipping invalid record", line_no=42, reason='missing_acct_id')
job_logger.error("Database connection failed", host='db2.company.com', port=50000)
job_logger.critical("Disk space exhausted", available_mb=10)

# 5. Use batch-specific methods
job_logger.log_job_start('CUSTOMER_LOAD', run_id)
job_logger.log_batch_start(batch_size=5000, batch_num=1)
job_logger.log_batch_complete(batch_num=1, records_processed=5000, duration_ms=1234.5)
job_logger.log_db_operation('INSERT', 'STG_CUSTOMER', row_count=5000, duration_ms=890.2)
job_logger.log_job_complete(status='SUCCESS', total_records=50000, duration_seconds=120.5)

# 6. Exception handling
try:
    risky_operation()
except Exception as e:
    job_logger.exception("Operation failed")  # Auto-captures traceback
    raise

# 7. Using decorator
@log_execution(job_logger, level='INFO')
def process_file(file_path: str) -> int:
    # Function entry/exit/exception logged automatically
    return process(file_path)
```

### 5. Sample Log Output (JSON)

```json
{"timestamp": "2025-12-01T10:30:45.123456+00:00", "level": "INFO", "logger": "CUSTOMER_LOAD", "message": "Job started: CUSTOMER_LOAD", "run_id": "20251201_103045", "job_name": "CUSTOMER_LOAD", "event_type": "job_start", "start_time": "2025-12-01T10:30:45.123456+00:00"}
{"timestamp": "2025-12-01T10:30:45.234567+00:00", "level": "DEBUG", "logger": "CUSTOMER_LOAD", "message": "Reading configuration", "run_id": "20251201_103045", "job_name": "CUSTOMER_LOAD", "config_file": "config/prod.yaml"}
{"timestamp": "2025-12-01T10:30:46.345678+00:00", "level": "INFO", "logger": "CUSTOMER_LOAD", "message": "Processing batch 1", "run_id": "20251201_103045", "job_name": "CUSTOMER_LOAD", "batch_num": 1, "batch_size": 5000, "event_type": "batch_start"}
{"timestamp": "2025-12-01T10:30:47.456789+00:00", "level": "WARNING", "logger": "CUSTOMER_LOAD", "message": "Validation error at line 42: ACCT_ID - missing required field", "run_id": "20251201_103045", "job_name": "CUSTOMER_LOAD", "line_no": 42, "field": "ACCT_ID", "event_type": "validation_error"}
{"timestamp": "2025-12-01T10:30:48.567890+00:00", "level": "ERROR", "logger": "CUSTOMER_LOAD", "message": "Database connection failed", "run_id": "20251201_103045", "job_name": "CUSTOMER_LOAD", "host": "db2.company.com", "port": 50000, "exception": "Traceback..."}
```

## Configuration

```yaml
# config/base_config.yaml
logging:
  level: "INFO"                    # DEBUG, INFO, WARNING, ERROR, CRITICAL
  format: "json"                   # json | text
  output: "both"                   # console | file | both
  
  file:
    path: "logs/batch_{date}.log"  # {date} replaced with YYYYMMDD
    max_size_mb: 100               # Rotate at this size
    backup_count: 10               # Keep this many backups
    encoding: "utf-8"
  
  console:
    enabled: true
    colored: false                 # Color output (dev only)
  
  # Fields to always include
  default_context:
    application: "MainframeBatchModernization"
    environment: "prod"
```

## Integration with Splunk/ELK

### Splunk Query Examples
```spl
# Find all errors for a specific run
index=batch_logs level=ERROR run_id="20251201_103045"

# Count records processed per batch
index=batch_logs event_type="batch_complete" | stats sum(records_processed) by job_name

# Identify slow batches (>5 seconds)
index=batch_logs event_type="batch_complete" duration_ms>5000 | table run_id, batch_num, duration_ms

# Track validation errors by field
index=batch_logs event_type="validation_error" | stats count by field, validation_error
```

### ELK (Elasticsearch) Query Examples
```json
// Find all errors for a job
{
  "query": {
    "bool": {
      "must": [
        { "term": { "job_name": "CUSTOMER_LOAD" } },
        { "term": { "level": "ERROR" } }
      ]
    }
  }
}
```

## Common Pitfalls

❌ **Print statements instead of logging**
```python
# Bad
print(f"Processing record {i}")  # Lost in production, no context
```

✅ **Structured logging**
```python
# Good
logger.debug("Processing record", record_num=i, acct_id=record.acct_id)
```

❌ **Missing context**
```python
# Bad
logger.error("Insert failed")  # Which run? Which batch?
```

✅ **Context-rich logging**
```python
# Good
logger.error("Insert failed", run_id=run_id, batch_num=batch_num, 
             table='STG_CUSTOMER', row_count=5000)
```

❌ **Logging sensitive data**
```python
# Bad - PII exposure
logger.info(f"Processing customer {customer_ssn}")
```

✅ **Mask sensitive data**
```python
# Good
logger.info("Processing customer", customer_id_masked=mask_pii(customer_id))
```

❌ **Wrong log level**
```python
# Bad - DEBUG in production fills logs
logger.debug("Processing each record")  # Called millions of times
```

✅ **Appropriate log levels**
```python
# Good - INFO for batches, DEBUG for development
logger.info("Batch complete", batch_num=i, records=5000)  # Once per batch
```

## Quick Reference

| Level | When to Use | Example |
|-------|-------------|---------|
| `DEBUG` | Detailed diagnostic info (dev/troubleshooting) | Field values, loop iterations |
| `INFO` | Normal operational messages | Job start, batch complete, record counts |
| `WARNING` | Unexpected but handled situations | Skipped records, retry attempts |
| `ERROR` | Error conditions needing attention | DB failures, validation failures |
| `CRITICAL` | Severe errors requiring immediate action | Disk full, security breach |

## Testing

```python
import pytest
from io import StringIO
import json

def test_batch_logger_levels():
    """Test all log levels work correctly."""
    stream = StringIO()
    handler = logging.StreamHandler(stream)
    
    logger = BatchLogger('test', level='DEBUG', console_output=False)
    logger._logger.addHandler(handler)
    
    logger.debug("Debug message")
    logger.info("Info message")
    logger.warning("Warning message")
    logger.error("Error message")
    logger.critical("Critical message")
    
    output = stream.getvalue()
    assert "Debug message" in output
    assert "Info message" in output
    assert "Critical message" in output

def test_context_included_in_logs():
    """Test that context is included in all log entries."""
    stream = StringIO()
    handler = logging.StreamHandler(stream)
    handler.setFormatter(JSONFormatter())
    
    logger = BatchLogger('test', level='DEBUG', console_output=False)
    logger._logger.addHandler(handler)
    logger.set_context(run_id='TEST_001', job_name='TEST_JOB')
    
    logger.info("Test message")
    
    output = stream.getvalue()
    log_entry = json.loads(output.strip())
    
    assert log_entry['run_id'] == 'TEST_001'
    assert log_entry['job_name'] == 'TEST_JOB'
    assert log_entry['message'] == 'Test message'
```

## Next Steps

- **Security**: See `07-security-compliance.md` for PII masking in logs
- **Performance**: Monitor log volume, adjust levels per environment
- **Alerting**: Configure alerts on ERROR/CRITICAL levels



# Repository Pattern for DB2

## Concept

The repository pattern abstracts data access, providing a clean interface between business logic and database operations. In the **hybrid producer-consumer pipeline**, each DB Writer thread has its own dedicated connection (not shared across threads). This enables high-performance DB2 integration while maintaining testability.

**Why threading for DB Writers?**
- DB I/O releases the GIL during network operations
- Threads are lighter weight than processes
- Shared memory makes metrics aggregation easier
- No IPC overhead for queue communication

## Architecture Context

```
┌─────────────────────────────────────────────────────────────┐
│                    PIPELINE CONTEXT                         │
├─────────────────────────────────────────────────────────────┤
│  DB Writer 1  ──────►  Own Connection  ──────►  STAGING     │
│  (Thread)                                                   │
│  DB Writer 2  ──────►  Own Connection  ──────►  TABLE       │
│  (Thread)                                                   │
│  DB Writer M  ──────►  Own Connection  ──────►  (RUN_ID)    │
│  (Thread)                                                   │
├─────────────────────────────────────────────────────────────┤
│  After pipeline completes:                                  │
│  Main Process  ──────►  Single Conn  ──────►  MERGE to FINAL│
└─────────────────────────────────────────────────────────────┘
```

**Key Points:**
- Each DB Writer thread creates its **own connection** (not shared)
- Threading works for DB I/O because GIL is released during network calls
- Staging table includes `RUN_ID` and `WORKER_ID` for traceability
- MERGE to final happens **after** all writers complete
- Retry logic with exponential backoff in each writer

## When to Use

✅ **Use when:**
- Using producer-consumer pipeline with multiple DB writer threads
- Inserting large volumes of data to DB2 (100K+ records)
- Need per-worker connection management
- Require retry logic for transient failures
- Want testable data access layer (mockable)
- Using staging-first pattern with RUN_ID + WORKER_ID

❌ **Consider alternatives when:**
- Simple single-record CRUD operations
- Using ORM (SQLAlchemy, Django ORM)
- Single-process batch jobs (connection pool may be sufficient)

## Complete Implementation

### 1. Connection Factory (Per-Worker)

```python
import ibm_db_dbi
from typing import Optional
from dataclasses import dataclass

@dataclass
class ConnectionConfig:
    """DB2 connection configuration."""
    database: str
    hostname: str
    port: int
    uid: str
    pwd: str
    protocol: str = 'TCPIP'
    security: Optional[str] = None  # SSL | None
    
    def to_connection_string(self) -> str:
        """Generate DB2 connection string."""
        params = [
            f"DATABASE={self.database}",
            f"HOSTNAME={self.hostname}",
            f"PORT={self.port}",
            f"UID={self.uid}",
            f"PWD={self.pwd}",
            f"PROTOCOL={self.protocol}"
        ]
        
        if self.security:
            params.append(f"SECURITY={self.security}")
        
        return ';'.join(params)


class ConnectionFactory:
    """
    Factory for creating DB2 connections.
    
    In pipeline mode, each DbWriter thread calls create() once to get
    its own dedicated connection. Connections are NOT shared across threads.
    """
    
    def __init__(self, config: ConnectionConfig):
        self._config = config
        self._connection_string = config.to_connection_string()
    
    def create(self):
        """
        Create a new DB2 connection.
        
        Each DB Writer thread should call this once at startup.
        The connection lives for the lifetime of the worker thread.
        
        Returns:
            DB2 connection object (ibm_db_dbi.Connection)
        
        Raises:
            Exception: If connection fails
        """
        try:
            conn = ibm_db_dbi.connect(self._connection_string, '', '')
            conn.autocommit = False  # Explicit transaction control
            return conn
        except Exception as e:
            raise Exception(f"Failed to connect to DB2: {e}") from e
    
    def test_connection(self) -> bool:
        """Test if connection is working."""
        try:
            conn = self.create()
            cursor = conn.cursor()
            cursor.execute("SELECT 1 FROM SYSIBM.SYSDUMMY1")
            cursor.fetchone()
            cursor.close()
            conn.close()
            return True
        except:
            return False
```

### 2. Connection Pool (For Single-Process Mode Only)

```python
from queue import Queue, Empty
from threading import Lock
import time
import logging

logger = logging.getLogger(__name__)


class PooledConnection:
    """Wrapper for pooled connection with tracking."""
    
    def __init__(self, connection, pool):
        self.connection = connection
        self.pool = pool
        self.in_use = False
        self.created_at = time.time()
        self.last_used = time.time()
    
    def __enter__(self):
        return self.connection
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.pool.return_connection(self)
    
    def is_stale(self, max_idle_time: int) -> bool:
        """Check if connection has been idle too long."""
        return (time.time() - self.last_used) > max_idle_time


class ConnectionPool:
    """
    Thread-safe DB2 connection pool.
    Pre-creates minimum connections, expands up to maximum, reuses connections.
    """
    
    def __init__(self, 
                 factory: ConnectionFactory,
                 min_size: int = 2,
                 max_size: int = 10,
                 max_idle_time: int = 300,
                 connection_timeout: int = 30):
        """
        Args:
            factory: ConnectionFactory for creating connections
            min_size: Minimum connections to maintain
            max_size: Maximum connections allowed
            max_idle_time: Seconds before closing idle connection
            connection_timeout: Seconds to wait for available connection
        """
        self._factory = factory
        self._min_size = min_size
        self._max_size = max_size
        self._max_idle_time = max_idle_time
        self._connection_timeout = connection_timeout
        
        self._pool: Queue[PooledConnection] = Queue(maxsize=max_size)
        self._current_size = 0
        self._lock = Lock()
        
        # Pre-create minimum connections
        for _ in range(min_size):
            conn = factory.create()
            pooled = PooledConnection(conn, self)
            self._pool.put(pooled)
            self._current_size += 1
        
        logger.info(f"Connection pool initialized: min={min_size}, max={max_size}")
    
    def get_connection(self) -> PooledConnection:
        """
        Get a connection from the pool.
        Creates new connection if pool is empty and under max_size.
        
        Returns:
            PooledConnection (use with context manager)
        
        Raises:
            Exception: If pool exhausted or timeout
        """
        try:
            # Try to get existing connection
            pooled_conn = self._pool.get(timeout=self._connection_timeout)
            
            # Check if connection is stale
            if pooled_conn.is_stale(self._max_idle_time):
                logger.debug("Closing stale connection, creating new one")
                try:
                    pooled_conn.connection.close()
                except:
                    pass
                pooled_conn.connection = self._factory.create()
                pooled_conn.created_at = time.time()
            
            pooled_conn.in_use = True
            pooled_conn.last_used = time.time()
            return pooled_conn
            
        except Empty:
            # Pool is empty, try to create new connection
            with self._lock:
                if self._current_size < self._max_size:
                    conn = self._factory.create()
                    pooled = PooledConnection(conn, self)
                    pooled.in_use = True
                    self._current_size += 1
                    logger.debug(f"Created new connection: pool size={self._current_size}")
                    return pooled
                else:
                    raise Exception(
                        f"Connection pool exhausted: {self._current_size} connections in use"
                    )
    
    def return_connection(self, pooled_conn: PooledConnection) -> None:
        """Return connection to pool."""
        pooled_conn.in_use = False
        pooled_conn.last_used = time.time()
        self._pool.put(pooled_conn)
    
    def close_all(self) -> None:
        """Close all connections in pool. Call on shutdown."""
        logger.info("Closing all pool connections")
        while not self._pool.empty():
            try:
                pooled = self._pool.get_nowait()
                pooled.connection.close()
            except:
                pass
        self._current_size = 0
    
    def get_stats(self) -> dict:
        """Get pool statistics."""
        return {
            'current_size': self._current_size,
            'max_size': self._max_size,
            'available': self._pool.qsize(),
            'in_use': self._current_size - self._pool.qsize()
        }
```

> **Note**: Connection pooling is for single-process mode. In the pipeline architecture, each DbWriter thread has its own dedicated connection.

### 3. Pipeline DB Writer (For Producer-Consumer Pattern)

```python
import threading
from queue import Queue
import queue
import time
from dataclasses import dataclass
from typing import List

@dataclass
class ProcessedBatch:
    """Batch from Processor ready for DB insert."""
    records: List[dict]
    worker_id: int
    batch_num: int


class DbWriter(threading.Thread):
    """
    DB Writer worker for producer-consumer pipeline.
    
    Why threading (not multiprocessing)?
    - DB I/O releases the GIL during network operations
    - Threads are lighter weight than processes (no IPC overhead)
    - Shared memory makes metrics aggregation easier
    - queue.Queue is thread-safe and simpler than multiprocessing.Queue
    
    Each DbWriter:
    - Runs in its own thread (threading.Thread)
    - Creates its own DB connection (not shared)
    - Takes batches from bounded queue
    - Inserts with retry on failure
    - Metrics accessible via get_result() after completion
    """
    
    def __init__(self, writer_id: int, input_queue: Queue,
                 db_config: dict, run_id: str, 
                 table_name: str = 'STG_BATCH'):
        super().__init__(name=f"DbWriter-{writer_id}", daemon=True)
        self.writer_id = writer_id
        self.input_queue = input_queue
        self.db_config = db_config
        self.run_id = run_id
        self.table_name = table_name
        
        # Metrics (thread-safe via GIL for simple increments)
        self.batches_written = 0
        self.records_written = 0
        self.errors = 0
        self._result = None
    
    def run(self) -> None:
        """
        Main processing loop - called in thread.
        
        Note: Returns None (thread method). Use get_result() after join().
        """
        # Create dedicated connection for this writer thread
        conn = self._create_connection()
        
        try:
            while True:
                try:
                    # Wait for batch from queue
                    batch = self.input_queue.get(timeout=30)
                    
                    if batch is None:  # Poison pill - shutdown signal
                        break
                    
                    # Insert batch with retry
                    self._insert_batch_with_retry(conn, batch)
                    
                    # Mark task done for join() support
                    self.input_queue.task_done()
                    
                except queue.Empty:
                    # Timeout - check if should continue
                    continue
                    
                except Exception as e:
                    self.errors += 1
                    # Log but continue processing
                    print(f"DbWriter-{self.writer_id} error: {e}")
        finally:
            conn.close()
        
        self._result = {
            'writer_id': self.writer_id,
            'batches_written': self.batches_written,
            'records_written': self.records_written,
            'errors': self.errors
        }
    
    def get_result(self) -> dict:
        """Get metrics after thread completes. Call after join()."""
        return self._result
    
    def _create_connection(self):
        """Create dedicated connection for this writer thread."""
        import ibm_db_dbi
        conn = ibm_db_dbi.connect(self.db_config['connection_string'])
        conn.autocommit = False
        return conn
    
    def _insert_batch_with_retry(self, conn, batch: ProcessedBatch,
                                  max_attempts: int = 3) -> None:
        """Insert batch with exponential backoff retry."""
        sql = f"""
            INSERT INTO {self.table_name} 
            (RUN_ID, WORKER_ID, ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT, INSERT_TS)
            VALUES (?, ?, ?, ?, ?, ?, CURRENT TIMESTAMP)
        """
        
        for attempt in range(1, max_attempts + 1):
            try:
                cursor = conn.cursor()
                
                # Prepare batch data including WORKER_ID for traceability
                rows = [
                    (self.run_id, self.writer_id, 
                     rec['acct_id'], rec['prod_code'],
                     float(rec['tran_amt']), rec['tran_date'])
                    for rec in batch.records
                ]
                
                cursor.executemany(sql, rows)
                conn.commit()
                cursor.close()
                
                self.batches_written += 1
                self.records_written += len(batch.records)
                return
                
            except Exception as e:
                conn.rollback()
                
                if attempt == max_attempts:
                    raise
                
                delay = 2 ** (attempt - 1)  # 1s, 2s, 4s
                print(f"DbWriter-{self.writer_id} retry {attempt}: {e}")
                time.sleep(delay)


def start_db_writers(num_writers: int, input_queue: Queue, 
                     db_config: dict, run_id: str) -> List[DbWriter]:
    """Start DbWriter threads. Returns list for later join() and get_result()."""
    writers = []
    for i in range(num_writers):
        writer = DbWriter(i, input_queue, db_config, run_id)
        writer.start()  # Thread.start()
        writers.append(writer)
    return writers
```

### 4. Retry Policy Decorator

```python
import time
from functools import wraps
from typing import Callable

class RetryPolicy:
    """
    Decorator for retrying operations with exponential backoff.
    Useful for transient DB2 errors (locks, deadlocks, connection issues).
    """
    
    def __init__(self, 
                 max_attempts: int = 3,
                 backoff_factor: float = 2.0,
                 initial_delay: float = 1.0,
                 max_delay: float = 30.0,
                 retryable_errors: tuple = None):
        """
        Args:
            max_attempts: Maximum retry attempts
            backoff_factor: Multiplier for delay (exponential backoff)
            initial_delay: Initial delay in seconds
            max_delay: Maximum delay between retries
            retryable_errors: Tuple of exception types to retry
        """
        self._max_attempts = max_attempts
        self._backoff_factor = backoff_factor
        self._initial_delay = initial_delay
        self._max_delay = max_delay
        self._retryable_errors = retryable_errors or (Exception,)
    
    def __call__(self, func: Callable) -> Callable:
        @wraps(func)
        def wrapper(*args, **kwargs):
            delay = self._initial_delay
            
            for attempt in range(1, self._max_attempts + 1):
                try:
                    return func(*args, **kwargs)
                    
                except self._retryable_errors as e:
                    if attempt == self._max_attempts:
                        logger.error(f"Max retry attempts reached for {func.__name__}: {e}")
                        raise
                    
                    # Calculate next delay with exponential backoff
                    current_delay = min(delay, self._max_delay)
                    logger.warning(
                        f"Attempt {attempt}/{self._max_attempts} failed for {func.__name__}, "
                        f"retrying in {current_delay:.1f}s: {e}"
                    )
                    time.sleep(current_delay)
                    delay *= self._backoff_factor
        
        return wrapper

# Usage example
@RetryPolicy(max_attempts=3, initial_delay=1.0)
def insert_with_retry(repository, records, run_id):
    return repository.bulk_insert(records, run_id)
```

### 4. Staging Repository

```python
from typing import Iterable, List
from dataclasses import dataclass
from datetime import datetime

class Db2StagingRepository:
    """
    Repository for staging table operations.
    Handles bulk inserts with batching for high throughput.
    """
    
    def __init__(self, 
                 connection_pool: ConnectionPool,
                 table_name: str = 'STG_BATCH',
                 batch_size: int = 5000):
        """
        Args:
            connection_pool: ConnectionPool instance
            table_name: Staging table name
            batch_size: Records per batch insert (5K-10K optimal)
        """
        self._pool = connection_pool
        self._table_name = table_name
        self._batch_size = batch_size
    
    def bulk_insert(self, records: Iterable, run_id: str) -> int:
        """
        Bulk insert records to staging table.
        Uses executemany for high performance (100x faster than individual inserts).
        
        Args:
            records: Iterable of domain model records
            run_id: Run identifier for traceability
        
        Returns:
            Total number of records inserted
        """
        sql = f"""
            INSERT INTO {self._table_name} 
            (RUN_ID, ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT, INSERT_TS)
            VALUES (?, ?, ?, ?, ?, CURRENT TIMESTAMP)
        """
        
        pooled_conn = self._pool.get_connection()
        
        try:
            conn = pooled_conn.connection
            conn.autocommit = False
            cursor = conn.cursor()
            
            batch = []
            total_inserted = 0
            
            for record in records:
                # Convert domain model to tuple for DB insert
                batch.append((
                    run_id,
                    record.acct_id,
                    record.prod_code,
                    float(record.tran_amt),  # Decimal → float for DB2
                    record.tran_date
                ))
                
                # Insert batch when size reached
                if len(batch) >= self._batch_size:
                    cursor.executemany(sql, batch)
                    conn.commit()
                    total_inserted += len(batch)
                    logger.info(f"Inserted batch: {len(batch)} records, total: {total_inserted}")
                    batch.clear()
            
            # Insert remaining records
            if batch:
                cursor.executemany(sql, batch)
                conn.commit()
                total_inserted += len(batch)
                logger.info(f"Inserted final batch: {len(batch)} records, total: {total_inserted}")
            
            cursor.close()
            return total_inserted
            
        except Exception as e:
            conn.rollback()
            logger.error(f"Bulk insert failed: {e}")
            raise
        finally:
            self._pool.return_connection(pooled_conn)
    
    def get_record_count(self, run_id: str) -> int:
        """Get count of records for specific RUN_ID."""
        sql = f"SELECT COUNT(*) FROM {self._table_name} WHERE RUN_ID = ?"
        
        pooled_conn = self._pool.get_connection()
        try:
            cursor = pooled_conn.connection.cursor()
            cursor.execute(sql, (run_id,))
            count = cursor.fetchone()[0]
            cursor.close()
            return count
        finally:
            self._pool.return_connection(pooled_conn)
    
    def clear_run(self, run_id: str) -> int:
        """Clear staging records for specific RUN_ID (for restartability)."""
        sql = f"DELETE FROM {self._table_name} WHERE RUN_ID = ?"
        
        pooled_conn = self._pool.get_connection()
        try:
            conn = pooled_conn.connection
            cursor = conn.cursor()
            cursor.execute(sql, (run_id,))
            deleted = cursor.rowcount
            conn.commit()
            cursor.close()
            logger.info(f"Cleared {deleted} staging records for RUN_ID={run_id}")
            return deleted
        finally:
            self._pool.return_connection(pooled_conn)
```

### 5. Final Repository (with MERGE)

```python
class Db2FinalRepository:
    """
    Repository for final table operations.
    Uses MERGE for idempotent upserts.
    """
    
    def __init__(self,
                 connection_pool: ConnectionPool,
                 staging_table: str = 'STG_BATCH',
                 final_table: str = 'FINAL_BATCH'):
        self._pool = connection_pool
        self._staging_table = staging_table
        self._final_table = final_table
    
    def merge_from_staging(self, run_id: str) -> int:
        """
        Merge staging records into final table.
        Idempotent: can be re-run safely.
        
        Args:
            run_id: Run identifier
        
        Returns:
            Number of rows affected (inserts + updates)
        """
        merge_sql = f"""
            MERGE INTO {self._final_table} AS T
            USING (
                SELECT ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT
                FROM {self._staging_table}
                WHERE RUN_ID = ?
            ) AS S
            ON (T.ACCT_ID = S.ACCT_ID AND T.PROD_CD = S.PROD_CD)
            WHEN MATCHED THEN
                UPDATE SET 
                    T.TRAN_AMT = S.TRAN_AMT,
                    T.TRAN_DT = S.TRAN_DT,
                    T.LAST_RUN_ID = ?,
                    T.UPDATE_TS = CURRENT TIMESTAMP
            WHEN NOT MATCHED THEN
                INSERT (ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT, LAST_RUN_ID, UPDATE_TS)
                VALUES (S.ACCT_ID, S.PROD_CD, S.TRAN_AMT, S.TRAN_DT, ?, CURRENT TIMESTAMP)
        """
        
        pooled_conn = self._pool.get_connection()
        try:
            conn = pooled_conn.connection
            cursor = conn.cursor()
            cursor.execute(merge_sql, (run_id, run_id, run_id))
            affected = cursor.rowcount
            conn.commit()
            cursor.close()
            logger.info(f"MERGE completed: {affected} rows affected for RUN_ID={run_id}")
            return affected
        finally:
            self._pool.return_connection(pooled_conn)
    
    def get_final_count(self) -> int:
        """Get total count in final table."""
        sql = f"SELECT COUNT(*) FROM {self._final_table}"
        
        pooled_conn = self._pool.get_connection()
        try:
            cursor = pooled_conn.connection.cursor()
            cursor.execute(sql)
            count = cursor.fetchone()[0]
            cursor.close()
            return count
        finally:
            self._pool.return_connection(pooled_conn)
```

### 6. Complete Usage Example

```python
from datetime import datetime
from decimal import Decimal

# 1. Setup connection configuration
config = ConnectionConfig(
    database='PROD_DB',
    hostname='db2.company.com',
    port=50000,
    uid='batch_user',
    pwd='secret_password',
    security='SSL'
)

# 2. Create connection factory and pool
factory = ConnectionFactory(config)
pool = ConnectionPool(factory, min_size=2, max_size=10)

# 3. Initialize repositories
staging_repo = Db2StagingRepository(pool, batch_size=5000)
final_repo = Db2FinalRepository(pool)

# 4. Generate RUN_ID
run_id = datetime.now().strftime('%Y%m%d_%H%M%S')

# 5. Insert records to staging (with retry)
@RetryPolicy(max_attempts=3)
def insert_batch(records):
    return staging_repo.bulk_insert(records, run_id)

try:
    total_inserted = insert_batch(records)
    logger.info(f"Inserted {total_inserted} records to staging")
    
    # 6. Merge to final table
    affected = final_repo.merge_from_staging(run_id)
    logger.info(f"Merged {affected} records to final table")
    
except Exception as e:
    logger.error(f"Batch processing failed: {e}")
    raise
finally:
    # 7. Cleanup
    pool.close_all()
```

## Configuration

```yaml
# config/base_config.yaml
db2:
  driver: "ibm_db_dbi"
  
  connection:
    database: "PROD_DB"
    hostname: "db2.company.com"
    port: 50000
    protocol: "TCPIP"
    security: "SSL"  # Optional
  
  connection_pool:
    min_size: 2
    max_size: 10
    max_idle_time: 300
    connection_timeout: 30
  
  repository:
    batch_size: 5000
    staging_table: "STG_BATCH"
    final_table: "FINAL_BATCH"
  
  retry:
    max_attempts: 3
    backoff_factor: 2.0
    initial_delay: 1.0
```

## Common Pitfalls

❌ **Individual inserts**
```python
# Bad - 100x slower
for record in records:
    cursor.execute(sql, (record.field1, record.field2))
    conn.commit()
```

✅ **Batch inserts**
```python
# Good - 100x faster
cursor.executemany(sql, [(r.field1, r.field2) for r in records])
conn.commit()
```

❌ **No connection pooling**
```python
# Bad - connection overhead
conn = ibm_db_dbi.connect(conn_string)
cursor = conn.cursor()
# ... use connection
conn.close()
```

✅ **Use connection pool**
```python
# Good - reuse connections
pooled = pool.get_connection()
with pooled as conn:
    cursor = conn.cursor()
    # ... use connection
# Auto-returned to pool
```

❌ **Autocommit mode**
```python
# Bad - can't rollback
conn.autocommit = True
```

✅ **Explicit transactions**
```python
# Good - transactional control
conn.autocommit = False
try:
    # ... operations
    conn.commit()
except:
    conn.rollback()
```

## Testing

```python
import pytest
from unittest.mock import Mock, MagicMock

@pytest.fixture
def mock_connection_pool():
    pool = Mock()
    pooled_conn = Mock()
    conn = MagicMock()
    cursor = MagicMock()
    
    conn.cursor.return_value = cursor
    pooled_conn.connection = conn
    pool.get_connection.return_value = pooled_conn
    
    return pool, conn, cursor

def test_bulk_insert(mock_connection_pool):
    pool, conn, cursor = mock_connection_pool
    
    repo = Db2StagingRepository(pool, batch_size=2)
    
    records = [
        Mock(acct_id='A1', prod_code='P1', tran_amt=Decimal('100'), tran_date='2025-01-01'),
        Mock(acct_id='A2', prod_code='P2', tran_amt=Decimal('200'), tran_date='2025-01-02'),
    ]
    
    count = repo.bulk_insert(records, 'TEST_RUN_001')
    
    assert count == 2
    assert cursor.executemany.call_count == 1
    assert conn.commit.call_count == 1
```

## Next Steps

- **Reconciliation**: See `04-reconciliation.md` for data validation
- **Logging**: See `05-logging-monitoring.md` for structured logging
- **Performance**: See `08-performance-tuning.md` for optimization



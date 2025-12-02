# Mainframe to Python Batch Modernization - Copilot Instructions

> **Context**: This project modernizes mainframe COBOL batch jobs to distributed Python, handling EBCDIC files, COBOL copybooks, and DB2 integration.

> **Python Version**: **3.12.3** required (for `match` statements, improved typing, and better multiprocessing).

> **⚠️ Critical**: Input files can exceed **100GB+**. All file processing MUST use chunked reading with producer-consumer pipeline. Memory footprint should remain **< 500MB** regardless of file size.

---

## Reading Mode Selection

The pipeline supports **two reading modes**, configured via `pipeline.reading_mode`:

| Mode | Config | When to Use |
|------|--------|-------------|
| **Single Reader** | `reading_mode: "single"` | Default. Network storage, variable-length records, simplicity |
| **Multi-Reader** | `reading_mode: "multi"` | Local SSD, fixed-length records, pre-split files available |

```yaml
# Single reader (default, recommended)
pipeline:
  reading_mode: "single"

# Multi-reader (requires pre-split chunk files)
pipeline:
  reading_mode: "multi"
  multi_reader:
    input_pattern: "/data/chunks/chunk_*.dat"
```

---

## Core Architecture: Hybrid Producer-Consumer Pipeline

The architecture uses **hybrid parallelism**:
- **Processors**: `multiprocessing` (CPU-bound work, must bypass GIL)
- **DB Writers**: `threading` (I/O-bound, GIL released during network I/O)
- **Queue Bridge**: `mp.Manager().Queue()` → adapter thread → `queue.Queue()` bridges process/thread boundary

### Pattern A: Single Reader (Default)

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      MAINFRAME SOURCE (100GB+)                           │
└───────────────────────────────┬─────────────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │    SINGLE READER      │
                    │    (Main Thread)      │
                    │  64KB chunks          │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │   RAW BATCHES QUEUE   │
                    │ (multiprocessing.Queue)│
                    └───────────┬───────────┘
                                │
        ┌───────────────────────┼───────────────────────┐
        ▼                       ▼                       ▼
┌───────────────┐       ┌───────────────┐       ┌───────────────┐
│  PROCESSOR 1  │       │  PROCESSOR 2  │       │  PROCESSOR N  │
│  (Process)    │       │  (Process)    │       │  (Process)    │
│───────────────│       │───────────────│       │───────────────│
│ Parse,        │       │ Parse,        │       │ Parse,        │
│ Validate,     │       │ Validate,     │       │ Validate,     │
│ Transform     │       │ Transform     │       │ Transform     │
│ (CPU-bound)   │       │ (CPU-bound)   │       │ (CPU-bound)   │
└───────┬───────┘       └───────┬───────┘       └───────┬───────┘
        │ multiprocessing       │                       │
        └───────────────────────┼───────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │  MP MANAGER QUEUE     │
                    │ (cross-process safe)  │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │   QUEUE ADAPTER       │
                    │   (Bridge Thread)     │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │  WRITER QUEUE         │
                    │  (queue.Queue)        │
                    └───────────┬───────────┘
                                │
                ┌───────────────┼───────────────┐
                ▼               ▼               ▼
        ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
        │  DB WRITER 1  │ │  DB WRITER 2  │ │  DB WRITER M  │
        │  (Thread)     │ │  (Thread)     │ │  (Thread)     │
        │───────────────│ │───────────────│ │───────────────│
        │ Own DB conn   │ │ Own DB conn   │ │ Own DB conn   │
        │ (I/O-bound)   │ │ (I/O-bound)   │ │ (I/O-bound)   │
        └───────────────┘ └───────────────┘ └───────────────┘
                │ threading     │                 │
                └───────────────┼─────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │   DB2 STAGING TABLE   │
                    └───────────────────────┘
```

### Pattern B: Multi-Reader (Pre-Split Files Required)

```
┌─────────────────────────────────────────────────────────────────────────┐
│        PRE-SPLIT CHUNK FILES (each reader has exclusive file)           │
│   chunk_001.dat    chunk_002.dat    chunk_003.dat    chunk_004.dat      │
└───────────┬───────────────┬───────────────┬───────────────┬─────────────┘
            │               │               │               │
            ▼               ▼               ▼               ▼
    ┌───────────────┐ ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
    │ READER/PROC 1 │ │ READER/PROC 2 │ │ READER/PROC 3 │ │ READER/PROC 4 │
    │  (Process)    │ │  (Process)    │ │  (Process)    │ │  (Process)    │
    │  own file     │ │  own file     │ │  own file     │ │  own file     │
    │  (CPU-bound)  │ │  (CPU-bound)  │ │  (CPU-bound)  │ │  (CPU-bound)  │
    └───────┬───────┘ └───────┬───────┘ └───────┬───────┘ └───────┬───────┘
            │ multiprocessing │               │               │
            └───────────────┴───────┬───────┴───────────────┘
                                    │
                        ┌───────────▼───────────┐
                        │   MP MANAGER QUEUE    │
                        │  (cross-process safe) │
                        └───────────┬───────────┘
                                    │
                        ┌───────────▼───────────┐
                        │   QUEUE ADAPTER       │
                        │   (Bridge Thread)     │
                        └───────────┬───────────┘
                                    │
                        ┌───────────▼───────────┐
                        │    WRITER QUEUE       │
                        │    (queue.Queue)      │
                        └───────────┬───────────┘
                                    │
                    ┌───────────────┼───────────────┐
                    ▼               ▼               ▼
            ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
            │  DB WRITER 1  │ │  DB WRITER 2  │ │  DB WRITER M  │
            │  (Thread)     │ │  (Thread)     │ │  (Thread)     │
            │ Own DB conn   │ │ Own DB conn   │ │ Own DB conn   │
            └───────────────┘ └───────────────┘ └───────────────┘
                    │ threading     │                 │
                    └───────────────┼─────────────────┘
                                    │
                        ┌───────────▼───────────┐
                        │   DB2 STAGING TABLE   │
                        └───────────────────────┘
```

**Key Principles:**
1. **Single reader mode**: No file contention, works with any storage
2. **Multi-reader mode**: Requires pre-split files (use `FileSplitter`)
3. **Processors use multiprocessing**: CPU-bound work, bypasses Python GIL
4. **DB Writers use threading**: I/O-bound, GIL released during network I/O
5. **Chunk read files** (64KB buffer) - 100x fewer I/O syscalls
6. **One DB connection per writer thread** - Not shared across threads
7. Batch DB inserts using `executemany` (5K records per batch)
8. Use staging tables with `RUN_ID + WORKER_ID` for traceability

---

## Project Structure Template

```
batch_project/
  config/
    base_config.yaml              # Shared settings (reading_mode, workers)
    {env}_config.yaml             # Environment overrides (dev/test/prod)
    secrets/                      # .gitignored - vault-managed
      {env}_secrets.yaml
  
  src/
    core/                         # Business logic (NO I/O, NO DB)
      models.py                   # Domain models (@dataclass)
      validators.py               # Business rules validation
      services/
        transformation_service.py
        reconciliation_service.py
    
    io_layer/                     # File I/O (EBCDIC, copybooks)
      ebcdic_reader.py            # Chunked EBCDIC reading
      copybook_parser.py
      packed_decimal.py           # COMP-3 conversion
      file_splitter.py            # Split files for multi-reader mode
      file_writer.py
    
    pipeline/                     # Producer-Consumer Pipeline
      orchestrator.py             # PipelineOrchestrator (mode selection)
      single_reader.py            # SingleReader + Processor workers
      chunk_file_reader.py        # ChunkFileReader for multi-reader mode
      db_writer.py                # DbWriter worker
    
    repository/                   # DB2 access layer
      connection_factory.py       # One connection per writer
      staging_repository.py       # Bulk insert to staging
      final_repository.py         # MERGE to final
      retry_policy.py             # Exponential backoff
    
    batch/                        # Application entry point
      app.py                      # Main entry, post-pipeline MERGE
      shutdown_handler.py         # Graceful shutdown
    
    util/                         # Cross-cutting concerns
      config_loader.py
      worker_logger.py            # Per-worker structured logging
      metrics_aggregator.py       # Aggregate worker metrics
      run_id.py
      pii_masker.py
  
  tests/
    unit/                         # Fast, isolated tests (70%)
    integration/                  # DB2 integration tests (20%)
    fixtures/                     # Test data, mocks
```

---

## Code Patterns & Templates

### 1. Domain Model (use @dataclass)

```python
from dataclasses import dataclass, field
from datetime import date
from decimal import Decimal
from typing import List

@dataclass
class InputRecord:
    line_no: int
    acct_id: str
    prod_code: str
    tran_amt: Decimal
    tran_date: date
    raw_line: str
    validation_errors: List[str] = field(default_factory=list)
    
    def mark_invalid(self, errors: List[str]) -> None:
        self.validation_errors = errors
```

### 2. EBCDIC File Reading (Chunked for 100GB+ Files)

```python
class EBCDICReader:
    """Chunked EBCDIC reader - 64KB buffer reduces I/O syscalls by 100x."""
    
    def __init__(self, file_path: str, encoding: str = 'cp037', 
                 record_length: int = None, chunk_size: int = 64 * 1024):
        self._file_path = file_path
        self._encoding = encoding  # cp037 (US), cp500 (Intl), cp1047 (Latin-1)
        self._record_length = record_length  # Fixed-width record length
        self._chunk_size = chunk_size  # 64KB read buffer
    
    def read_records(self) -> Iterator[str]:
        """Yield records using chunked I/O for efficiency."""
        if self._record_length:
            # Fixed-length: chunked reading
            buffer = b''
            with open(self._file_path, 'rb') as f:
                while True:
                    chunk = f.read(self._chunk_size)  # Read 64KB at once
                    if not chunk:
                        break
                    buffer += chunk
                    
                    while len(buffer) >= self._record_length:
                        raw_record = buffer[:self._record_length]
                        buffer = buffer[self._record_length:]
                        yield raw_record.decode(self._encoding).rstrip('\x00\n\r')
            
            if buffer:
                raise ValueError(f"Incomplete record: {len(buffer)} bytes")
        else:
            # Variable-length: Python's buffered I/O
            with open(self._file_path, 'rb') as f:
                for raw_line in f:
                    yield raw_line.decode(self._encoding).rstrip('\x00\n\r')
```

### 3. Packed Decimal (COMP-3) Conversion

```python
def unpack_comp3(packed_bytes: bytes, decimal_places: int = 0) -> Decimal:
    """Convert COBOL COMP-3 packed decimal to Python Decimal.
    
    Example: PIC S9(7)V99 COMP-3 → 5 bytes, decimal_places=2
    """
    last_byte = packed_bytes[-1]
    sign_nibble = last_byte & 0x0F
    is_negative = (sign_nibble == 0x0D)  # D = negative, C/F = positive
    
    digits = []
    for i, byte in enumerate(packed_bytes):
        high_nibble = (byte >> 4) & 0x0F
        low_nibble = byte & 0x0F
        digits.append(str(high_nibble))
        if i < len(packed_bytes) - 1:  # Skip sign nibble
            digits.append(str(low_nibble))
    
    value = Decimal(int(''.join(digits))) / (10 ** decimal_places)
    return -value if is_negative else value
```

### 4. Copybook Parser

```python
@dataclass
class CopybookField:
    name: str
    start_pos: int      # 0-based byte offset
    length: int         # Byte length
    data_type: str      # 'alphanumeric', 'numeric', 'comp3', 'comp'
    decimal_places: int = 0
    is_signed: bool = False

class CopybookParser:
    def __init__(self, fields: List[CopybookField]):
        self._fields = fields
    
    def parse(self, raw_bytes: bytes) -> Dict[str, Any]:
        result = {}
        for field in self._fields:
            field_bytes = raw_bytes[field.start_pos:field.start_pos + field.length]
            
            if field.data_type == 'alphanumeric':
                result[field.name] = field_bytes.decode('cp037').rstrip()
            elif field.data_type == 'comp3':
                result[field.name] = unpack_comp3(field_bytes, field.decimal_places)
            elif field.data_type == 'numeric':
                result[field.name] = int(field_bytes.decode('cp037').strip())
            
        return result
```

### 5. Repository Pattern (Staging)

```python
class Db2StagingRepository:
    def __init__(self, conn_factory, batch_size: int = 5000):
        self._conn_factory = conn_factory
        self._batch_size = batch_size
    
    def bulk_insert(self, records: Iterable[InputRecord], run_id: str) -> int:
        conn = self._conn_factory.create()
        conn.autocommit = False
        cur = conn.cursor()
        
        sql = """
            INSERT INTO STG_BATCH (RUN_ID, ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT)
            VALUES (?, ?, ?, ?, ?)
        """
        
        batch = []
        total = 0
        
        for rec in records:
            batch.append((run_id, rec.acct_id, rec.prod_code, 
                         float(rec.tran_amt), rec.tran_date))
            
            if len(batch) >= self._batch_size:
                cur.executemany(sql, batch)
                conn.commit()
                total += len(batch)
                batch.clear()
        
        if batch:
            cur.executemany(sql, batch)
            conn.commit()
            total += len(batch)
        
        cur.close()
        conn.close()
        return total
```

### 6. DB Writer (One Connection Per Worker Thread)

```python
import threading
from queue import Queue


class DbWriter(threading.Thread):
    """DB Writer for pipeline - each writer thread has its own connection.
    
    Why threading (not multiprocessing)?
    - DB I/O releases the GIL during network operations
    - Threads are lighter weight than processes
    - Shared memory makes metrics aggregation easier
    """
    
    def __init__(self, writer_id: int, input_queue: Queue, 
                 db_config: dict, run_id: str):
        super().__init__(name=f"DbWriter-{writer_id}", daemon=True)
        self.writer_id = writer_id
        self.input_queue = input_queue
        self.db_config = db_config
        self.run_id = run_id
        self._result = None
    
    def run(self) -> None:
        """Main loop - runs in separate thread."""
        # Each writer creates its OWN connection (not shared)
        conn = ibm_db_dbi.connect(self.db_config['connection_string'])
        repository = Db2StagingRepository(conn)
        
        batches_written = 0
        records_written = 0
        
        try:
            while True:
                try:
                    batch = self.input_queue.get(timeout=30)
                    if batch is None:  # Poison pill - shutdown
                        break
                    
                    repository.bulk_insert(batch.records, self.run_id, self.writer_id)
                    batches_written += 1
                    records_written += len(batch.records)
                    self.input_queue.task_done()
                    
                except queue.Empty:
                    continue
        finally:
            conn.close()
        
        self._result = {
            'writer_id': self.writer_id,
            'batches': batches_written,
            'records': records_written
        }
    
    def get_result(self) -> dict:
        """Get metrics after thread completes."""
        return self._result
```

### 7. Retry Policy with Exponential Backoff

```python
class RetryPolicy:
    def __init__(self, max_attempts: int = 3, backoff_factor: float = 2.0):
        self._max_attempts = max_attempts
        self._backoff_factor = backoff_factor
    
    def __call__(self, func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            for attempt in range(1, self._max_attempts + 1):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == self._max_attempts:
                        raise
                    delay = (self._backoff_factor ** (attempt - 1))
                    logger.warning(f"Attempt {attempt} failed, retrying in {delay}s...")
                    time.sleep(delay)
        return wrapper

# Usage:
@RetryPolicy(max_attempts=3)
def insert_records(repository, records, run_id):
    return repository.bulk_insert(records, run_id)
```

### 8. Reconciliation Pattern

```python
class ReconciliationService:
    def reconcile_counts(self, run_id: str, source_count: int) -> List[ReconciliationResult]:
        results = []
        
        # Check 1: Source vs Staging
        staging_count = self._staging_repo.get_record_count(run_id)
        results.append(ReconciliationResult(
            run_id=run_id,
            check_name='source_to_staging',
            expected_count=source_count,
            actual_count=staging_count,
            passed=(source_count == staging_count),
            discrepancy=staging_count - source_count
        ))
        
        # Check 2: Staging vs Final
        final_count = self._recon_repo.get_final_count_for_run(run_id)
        results.append(ReconciliationResult(
            run_id=run_id,
            check_name='staging_to_final',
            expected_count=staging_count,
            actual_count=final_count,
            passed=(staging_count == final_count),
            discrepancy=final_count - staging_count
        ))
        
        self._recon_repo.save_reconciliation_results(results)
        return results
```

### 9. Configuration Management

```python
class ConfigLoader:
    def __init__(self, config_dir: str, environment: str = "dev"):
        self._config_dir = Path(config_dir)
        self._environment = environment
        self._config = {}
    
    def load(self) -> Dict[str, Any]:
        # 1. Load base config
        base = self._load_yaml(self._config_dir / "base_config.yaml")
        self._config = base
        
        # 2. Merge environment-specific
        env_config = self._load_yaml(self._config_dir / f"{self._environment}_config.yaml")
        self._deep_merge(self._config, env_config)
        
        # 3. Load secrets
        secrets = self._load_yaml(self._config_dir / "secrets" / f"{self._environment}_secrets.yaml")
        self._deep_merge(self._config, secrets)
        
        # 4. Override with environment variables (BATCH_DB2_CONNECTION_POOL_MAX_SIZE)
        self._apply_env_var_overrides()
        
        return self._config
```

### 10. Structured Logging

```python
class JSONFormatter(logging.Formatter):
    def format(self, record: logging.LogRecord) -> str:
        log_data = {
            'timestamp': datetime.utcnow().isoformat() + 'Z',
            'level': record.levelname,
            'message': record.getMessage(),
            'run_id': getattr(record, 'run_id', None),
            'records_processed': getattr(record, 'records_processed', None)
        }
        if record.exc_info:
            log_data['exception'] = traceback.format_exception(*record.exc_info)
        return json.dumps(log_data)

# Usage with context
context_logger = ContextLogger(logger, run_id='20251130_120000', job_name='FILE_LOAD')
context_logger.info("Processing batch", records_processed=5000)
```

### 11. Graceful Shutdown

```python
class ShutdownHandler:
    def __init__(self):
        self._shutdown_requested = threading.Event()
        self._cleanup_callbacks = []
    
    def install_handlers(self):
        signal.signal(signal.SIGTERM, self.handle_signal)
        signal.signal(signal.SIGINT, self.handle_signal)
    
    def register_cleanup(self, callback: Callable):
        self._cleanup_callbacks.append(callback)
    
    def handle_signal(self, signum, frame):
        logger.warning(f"Received signal, shutting down gracefully...")
        self._shutdown_requested.set()
        for callback in self._cleanup_callbacks:
            callback()
        sys.exit(0)

# Usage:
shutdown_handler = ShutdownHandler()
shutdown_handler.install_handlers()
shutdown_handler.register_cleanup(lambda: connection_pool.close_all())

while not shutdown_handler.is_shutdown_requested():
    # Process records
    pass
```

### 12. PII Masking

```python
class PIIMasker:
    @staticmethod
    def mask_account_id(acct_id: str) -> str:
        """Show only last 4 digits."""
        if len(acct_id) <= 4:
            return '****'
        return '*' * (len(acct_id) - 4) + acct_id[-4:]
    
    @staticmethod
    def hash_pii(value: str, salt: str = None) -> str:
        """One-way hash for reconciliation without exposing data."""
        if salt:
            value = value + salt
        return hashlib.sha256(value.encode()).hexdigest()
```

---

## Configuration File Structure

### base_config.yaml
```yaml
application:
  name: "MainframeBatchModernization"
  log_level: "INFO"

batch:
  default_batch_size: 5000
  max_error_threshold: 100

file_processing:
  encoding: "cp037"  # EBCDIC code page
  line_terminator: "\n"

db2:
  driver: "ibm_db_dbi"
  connection_pool:
    min_size: 2
    max_size: 10
    max_idle_time: 300
  retry:
    max_attempts: 3
    backoff_factor: 2

parallelism:
  mode: "process"
  max_workers: 4

reconciliation:
  enabled: true
  count_check: true
  hash_validation: false
```

### secrets/{env}_secrets.yaml (NEVER commit to git)
```yaml
db2:
  connection_string: "DATABASE=PROD_DB;HOSTNAME=db2.company.com;PORT=50000;UID=user;PWD=secret"
  # OR use vault reference:
  vault:
    enabled: true
    url: "https://vault.company.com"
    secret_path: "secret/data/batch/db2/prod"
```

---

## Database Schema Patterns

### Staging Table (with RUN_ID)
```sql
CREATE TABLE STG_BATCH (
    STG_ID          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    RUN_ID          VARCHAR(50) NOT NULL,
    ACCT_ID         VARCHAR(10) NOT NULL,
    PROD_CD         VARCHAR(5) NOT NULL,
    TRAN_AMT        DECIMAL(15,2) NOT NULL,
    TRAN_DT         DATE NOT NULL,
    INSERT_TS       TIMESTAMP DEFAULT CURRENT TIMESTAMP
);
CREATE INDEX IDX_STG_RUN_ID ON STG_BATCH(RUN_ID);
```

### Final Table (with MERGE)
```sql
CREATE TABLE FINAL_BATCH (
    FINAL_ID        BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ACCT_ID         VARCHAR(10) NOT NULL,
    PROD_CD         VARCHAR(5) NOT NULL,
    TRAN_AMT        DECIMAL(15,2) NOT NULL,
    TRAN_DT         DATE NOT NULL,
    LAST_RUN_ID     VARCHAR(50),
    UPDATE_TS       TIMESTAMP,
    UNIQUE(ACCT_ID, PROD_CD)
);
```

### MERGE Statement
```sql
MERGE INTO FINAL_BATCH AS T
USING (
    SELECT ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT
    FROM STG_BATCH WHERE RUN_ID = ?
) AS S
ON (T.ACCT_ID = S.ACCT_ID AND T.PROD_CD = S.PROD_CD)
WHEN MATCHED THEN
    UPDATE SET T.TRAN_AMT = S.TRAN_AMT, T.TRAN_DT = S.TRAN_DT, T.UPDATE_TS = CURRENT TIMESTAMP
WHEN NOT MATCHED THEN
    INSERT (ACCT_ID, PROD_CD, TRAN_AMT, TRAN_DT) 
    VALUES (S.ACCT_ID, S.PROD_CD, S.TRAN_AMT, S.TRAN_DT);
```

### Control/Audit Table
```sql
CREATE TABLE BATCH_CONTROL (
    CONTROL_ID      INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    RUN_ID          VARCHAR(50) NOT NULL UNIQUE,
    JOB_NAME        VARCHAR(100) NOT NULL,
    START_TS        TIMESTAMP NOT NULL,
    END_TS          TIMESTAMP,
    STATUS          VARCHAR(20) CHECK (STATUS IN ('RUNNING', 'SUCCESS', 'FAILED')),
    RECORDS_READ    INTEGER,
    RECORDS_LOADED  INTEGER,
    ERROR_MSG       VARCHAR(1000)
);
```

### Reconciliation Audit Table
```sql
CREATE TABLE RECON_AUDIT (
    RECON_ID        INTEGER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    RUN_ID          VARCHAR(50) NOT NULL,
    CHECK_NAME      VARCHAR(50) NOT NULL,
    EXPECTED_CNT    INTEGER NOT NULL,
    ACTUAL_CNT      INTEGER NOT NULL,
    PASSED          CHAR(1) CHECK (PASSED IN ('Y', 'N')),
    DISCREPANCY     INTEGER NOT NULL,
    CHECK_TS        TIMESTAMP NOT NULL
);
```

---

## Testing Patterns

### Unit Test with Mocks
```python
@pytest.fixture
def mock_connection_factory():
    factory = Mock()
    conn = MagicMock()
    cursor = MagicMock()
    factory.create.return_value = conn
    conn.cursor.return_value = cursor
    return factory, conn, cursor

def test_bulk_insert(mock_connection_factory):
    factory, conn, cursor = mock_connection_factory
    repository = Db2StagingRepository(factory, batch_size=2)
    
    records = [
        InputRecord(1, 'ACCT001', 'LOAN', Decimal('100'), date.today(), 'raw1'),
        InputRecord(2, 'ACCT002', 'CARD', Decimal('200'), date.today(), 'raw2'),
    ]
    
    count = repository.bulk_insert(records, 'TEST_RUN_001')
    
    assert count == 2
    assert cursor.executemany.call_count == 1
    assert conn.commit.call_count == 1
```

### Integration Test
```python
@pytest.mark.integration
def test_end_to_end_load(db2_connection):
    run_id = 'TEST_E2E_001'
    
    staging_repo = Db2StagingRepository(db2_connection)
    final_repo = Db2FinalRepository(db2_connection)
    
    records = [InputRecord(...), InputRecord(...)]
    
    # Load to staging
    loaded = staging_repo.bulk_insert(records, run_id)
    assert loaded == len(records)
    
    # Merge to final
    merged = final_repo.merge_from_staging(run_id)
    assert merged == len(records)
```

---

## Common Data Type Mappings

| COBOL Type | Python Type | Conversion Notes |
|------------|-------------|------------------|
| `PIC X(n)` | `str` | EBCDIC decode, strip trailing spaces |
| `PIC 9(n)` | `int` | EBCDIC decode numeric text |
| `PIC S9(n)V99` | `Decimal` | Use Decimal for precision |
| `PIC 9(8)` (date) | `date` | Format: YYYYMMDD → `datetime.strptime('%Y%m%d')` |
| `PIC 9(6)` (time) | `time` | Format: HHMMSS → `datetime.strptime('%H%M%S')` |
| `COMP-3` | `Decimal` | Use `unpack_comp3()` utility |
| `COMP` | `int` | Binary: `int.from_bytes(bytes, 'big')` |

---

## Performance Tuning Guidelines

| Parameter | Starting Value | Adjust Based On |
|-----------|----------------|-----------------|
| `batch_size` | 5,000 | Increase to 10K for throughput; decrease if DB locks |
| `max_workers` | 4 | 1 per CPU core; monitor CPU usage |
| `connection_pool.max_size` | 10 | Increase if pool exhausted; watch DB connection limits |
| File partitions | 4 | Match worker count for parallel processing |

**Target Metrics:**
- Throughput: 10,000+ records/second
- CPU: < 70% average
- Memory: < 80% available
- DB Connections: < 50% of pool max

---

## Error Handling & Recovery

1. **Validation Errors**: Write to error file, continue processing
2. **DB Transient Errors**: Retry with exponential backoff (3 attempts)
3. **Fatal Errors**: Log, update BATCH_CONTROL status=FAILED, exit
4. **Restartability**: Clear staging for RUN_ID and re-run
5. **Idempotency**: MERGE statements are naturally idempotent

---

## Security Checklist

- [ ] PII masked in logs (use `PIIMasker`)
- [ ] Secrets in vault, not source control
- [ ] Audit logging enabled for data access
- [ ] DB user has least-privilege access
- [ ] Encryption in transit (DB2 SSL)
- [ ] Sensitive files encrypted at rest

---

## Quick Command Reference

```bash
# Run batch job
python -m src.batch.app --config config/ --env prod --file /data/input.dat

# Run with specific RUN_ID
python -m src.batch.app --run-id 20251130_120000

# Run reconciliation
python scripts/reconciliation/compare_counts.py --run-id 20251130_120000

# Run tests
pytest tests/unit -v
pytest tests/integration -v --db2-available
```

---

## Dependencies

```text
# Core
ibm-db>=3.1.0
ibm-db-dbi>=3.0.0
pyyaml>=6.0
python-dateutil>=2.8.0

# Configuration
jsonschema>=4.0.0
hvac>=1.0.0  # Vault client

# Copybook parsing (optional - choose one)
# cb2py>=1.0.0

# Utilities
cryptography>=41.0.0

# Testing
pytest>=7.4.0
pytest-cov>=4.1.0
pytest-mock>=3.11.0
```

---

## Key Implementation Rules

1. **Always use iterators** for file processing (never load entire file)
2. **Always batch DB operations** using `executemany` (never single inserts)
3. **Always use RUN_ID** for tracking and restartability
4. **Always validate before DB insert** (fail fast on bad data)
5. **Always reconcile counts** at every layer (source → staging → final)
6. **Always use connection pooling** (never create connections per operation)
7. **Always log with context** (RUN_ID, correlation IDs in structured logs)
8. **Always handle EBCDIC encoding** explicitly (never assume ASCII)
9. **Always use Decimal** for monetary amounts (never float)
10. **Always implement graceful shutdown** (cleanup connections, files)
11. **Always update requirements.txt** when adding new imports (pin versions with `>=`)

---

## When to Reference Full Guide

- **Detailed EBCDIC/Copybook implementation**: Section 6.3
- **Complete reconciliation framework**: Section 9
- **Testing strategy with examples**: Section 10
- **Security patterns and compliance**: Section 11
- **Migration strategy and cutover**: Section 13
- **Database schema details**: Appendix B



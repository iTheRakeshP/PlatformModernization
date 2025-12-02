# Core Architecture Overview

## Concept

Mainframe batch modernization follows a **hybrid producer-consumer pipeline architecture** that maximizes throughput for 100GB+ files. The architecture uses:

- **Multiprocessing** for CPU-bound work (parsing, validation, transformation)
- **Threading** for I/O-bound work (database writes)
- **Bounded queues** for backpressure control

> **⚠️ Critical Context**: Input files can exceed **100GB+** in size. All file processing MUST use chunked streaming patterns. Memory footprint should remain **< 500MB** regardless of file size.

## Parallelism Strategy

| Stage | Parallelism | Why |
|-------|-------------|-----|
| **File Reading** | Single process | Sequential I/O is disk-optimal, no contention |
| **Parse/Validate/Transform** | **`multiprocessing`** | CPU-bound work, must bypass Python GIL |
| **DB Writing** | **`threading`** | I/O-bound, GIL released during network I/O |

## Reading Mode Selection

The pipeline supports **two reading modes**, selected via configuration:

| Mode | When to Use | How It Works |
|------|-------------|--------------|
| **Single Reader** (default) | Most cases, network storage, variable-length records | One process reads sequentially, hands off to processor pool |
| **Multi-Reader** | Local SSD, fixed-length records, pre-split files available | Multiple processes read separate files in parallel |

```yaml
# config/base_config.yaml
pipeline:
  reading_mode: "single"    # "single" or "multi"
  
  # Single reader settings
  single_reader:
    chunk_size: 65536       # 64KB read buffer
    
  # Multi-reader settings (requires pre-split files)
  multi_reader:
    num_readers: 4
    input_pattern: "chunks/chunk_*.dat"  # Pre-split files
    # OR
    split_before_processing: true        # Split on-the-fly
    split_output_dir: "/tmp/splits"
```

## Pipeline Architecture Patterns

### Pattern A: Single Reader Pipeline (Default)

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      MAINFRAME SOURCE (100GB+)                           │
└───────────────────────────────┬─────────────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │    SINGLE READER      │
                    │    (Main Thread)      │
                    │  ─────────────────────│
                    │  • Sequential I/O     │
                    │  • 64KB chunks        │
                    │  • Disk-friendly      │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │   RAW BATCHES QUEUE   │
                    │  (multiprocessing.Queue)│
                    │   (max 20 batches)    │
                    └───────────┬───────────┘
                                │
        ┌───────────────────────┼───────────────────────┐
        ▼                       ▼                       ▼
┌───────────────┐       ┌───────────────┐       ┌───────────────┐
│  PROCESSOR 1  │       │  PROCESSOR 2  │       │  PROCESSOR N  │
│  (Process)    │       │  (Process)    │       │  (Process)    │
│───────────────│       │───────────────│       │───────────────│
│ • Parse       │       │ • Parse       │       │ • Parse       │
│ • Validate    │◄──────│ • Validate    │──────►│ • Validate    │
│ • Transform   │  CPU  │ • Transform   │  CPU  │ • Transform   │
│  (CPU-bound)  │ bound │  (CPU-bound)  │ bound │  (CPU-bound)  │
└───────┬───────┘       └───────┬───────┘       └───────┬───────┘
        │ multiprocessing       │                       │
        └───────────────────────┼───────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │  MP MANAGER QUEUE     │
                    │ (cross-process safe)  │
                    │  (max 10 batches)     │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │   QUEUE ADAPTER       │
                    │  (Bridge Thread)      │
                    │  mp.Queue → Queue     │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │   WRITER QUEUE        │
                    │  (queue.Queue)        │
                    │  (max 10 batches)     │
                    └───────────┬───────────┘
                                │
                ┌───────────────┼───────────────┐
                ▼               ▼               ▼
        ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
        │  DB WRITER 1  │ │  DB WRITER 2  │ │  DB WRITER M  │
        │  (Thread)     │ │  (Thread)     │ │  (Thread)     │
        │───────────────│ │───────────────│ │───────────────│
        │ • Own DB conn │ │ • Own DB conn │ │ • Own DB conn │
        │ • Bulk insert │◄│ • Bulk insert │►│ • Bulk insert │
        │  (I/O-bound)  │IO│  (I/O-bound)  │IO│  (I/O-bound)  │
        └───────────────┘  └───────────────┘  └───────────────┘
                │ threading        │                │
                └──────────────────┼────────────────┘
                                   │
                    ┌──────────────▼──────────────┐
                    │      DB2 STAGING TABLE      │
                    └─────────────────────────────┘
```

**Why This Hybrid Approach:**
- **Processors use multiprocessing**: Parse/validate/transform are CPU-bound; Python GIL would serialize threads
- **Writers use threading**: DB I/O releases GIL during network calls; threads are lighter weight
- **Queue bridge pattern**: `mp.Manager().Queue()` bridges process boundary to thread `queue.Queue()`
- **Shared memory for writers**: Easier metrics aggregation without IPC overhead

---

### Pattern B: Multi-Reader Pipeline (Pre-Split Files Required)

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      MAINFRAME SOURCE (100GB+)                           │
└───────────────────────────────┬─────────────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │    FILE SPLITTER      │
                    │  (Pre-processing)     │
                    │  ─────────────────────│
                    │  Split into N files   │
                    │  Each aligned to      │
                    │  record boundaries    │
                    └───────────┬───────────┘
                                │
        ┌─────────────┬─────────┼─────────┬─────────────┐
        ▼             ▼         ▼         ▼             ▼
   chunk_001.dat chunk_002.dat ... chunk_00N.dat
        │             │                   │
        ▼             ▼                   ▼
┌───────────────┐ ┌───────────────┐ ┌───────────────┐
│   READER/     │ │   READER/     │ │   READER/     │
│  PROCESSOR 1  │ │  PROCESSOR 2  │ │  PROCESSOR N  │
│  (Process)    │ │  (Process)    │ │  (Process)    │
│───────────────│ │───────────────│ │───────────────│
│  Own file     │ │  Own file     │ │  Own file     │
│  (CPU-bound)  │ │  (CPU-bound)  │ │  (CPU-bound)  │
└───────┬───────┘ └───────┬───────┘ └───────┬───────┘
        │ multiprocessing │                 │
        └─────────────────┼─────────────────┘
                          │
              ┌───────────▼───────────┐
              │    BOUNDED QUEUE      │
              │  (queue.Queue)        │
              │  (max 10 batches)     │
              └───────────┬───────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
   ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
   │  DB WRITER 1  │ │  DB WRITER 2  │ │  DB WRITER M  │
   │  (Thread)     │ │  (Thread)     │ │  (Thread)     │
   │───────────────│ │───────────────│ │───────────────│
   │ • Own DB conn │ │ • Own DB conn │ │ • Own DB conn │
   │  (I/O-bound)  │ │  (I/O-bound)  │ │  (I/O-bound)  │
   └───────────────┘ └───────────────┘ └───────────────┘
          │ threading     │                 │
          └───────────────┼─────────────────┘
                          │
              ┌───────────▼───────────┐
              │   DB2 STAGING TABLE   │
              └─────────────────────────┘
```

**Why This Hybrid Approach:**
- **Readers/Processors use multiprocessing**: Each has exclusive file, CPU-bound work
- **Writers use threading**: I/O-bound, GIL released during DB network calls
- **Queue adapter**: Bridges multiprocessing.Queue to queue.Queue for threads

**Requirements**:
- Files must be **pre-split** (no concurrent access to same file)
- Fixed-length records (for clean splits)
- Local/fast storage recommended

---

## File Splitting Strategy

When using multi-reader mode, files **MUST** be split before processing:

```python
from pathlib import Path
from typing import List
import os


class FileSplitter:
    """
    Split large file into chunks for multi-reader processing.
    
    MUST be run before multi-reader pipeline starts.
    Each chunk becomes a separate file - no concurrent access issues.
    """
    
    def __init__(self, record_length: int, target_chunk_size: int = 25 * 1024**3):
        """
        Args:
            record_length: Bytes per record (from copybook)
            target_chunk_size: Target size per chunk (default 25GB)
        """
        self.record_length = record_length
        self.target_chunk_size = target_chunk_size
    
    def split(self, input_file: str, output_dir: str) -> List[str]:
        """
        Split file into record-aligned chunks.
        
        Args:
            input_file: Path to large input file
            output_dir: Directory to write chunk files
        
        Returns:
            List of chunk file paths
        """
        Path(output_dir).mkdir(parents=True, exist_ok=True)
        
        # Calculate records per chunk (aligned to record boundary)
        records_per_chunk = self.target_chunk_size // self.record_length
        bytes_per_chunk = records_per_chunk * self.record_length
        
        chunk_files = []
        chunk_num = 0
        
        with open(input_file, 'rb') as src:
            while True:
                # Read one chunk worth of data
                chunk_data = src.read(bytes_per_chunk)
                if not chunk_data:
                    break
                
                # Write to chunk file
                chunk_path = Path(output_dir) / f"chunk_{chunk_num:04d}.dat"
                with open(chunk_path, 'wb') as dst:
                    dst.write(chunk_data)
                
                chunk_files.append(str(chunk_path))
                chunk_num += 1
                
                # Log progress
                print(f"Created {chunk_path.name} ({len(chunk_data):,} bytes)")
        
        return chunk_files
    
    def get_chunk_count(self, input_file: str) -> int:
        """Calculate how many chunks will be created."""
        file_size = os.path.getsize(input_file)
        records_per_chunk = self.target_chunk_size // self.record_length
        bytes_per_chunk = records_per_chunk * self.record_length
        return (file_size + bytes_per_chunk - 1) // bytes_per_chunk


class FileSplitValidator:
    """Validate that split files are ready for multi-reader processing."""
    
    @staticmethod
    def validate_chunks(chunk_files: List[str], record_length: int) -> bool:
        """
        Validate all chunk files are properly aligned.
        
        Returns:
            True if all chunks are valid
        """
        for chunk_file in chunk_files:
            size = os.path.getsize(chunk_file)
            if size % record_length != 0:
                raise ValueError(
                    f"Chunk {chunk_file} size {size} not aligned to "
                    f"record length {record_length}"
                )
        return True
    
    @staticmethod
    def find_existing_chunks(pattern: str) -> List[str]:
        """
        Find existing chunk files matching pattern.
        
        Args:
            pattern: Glob pattern like "chunks/chunk_*.dat"
        
        Returns:
            Sorted list of chunk file paths
        """
        from glob import glob
        chunks = sorted(glob(pattern))
        if not chunks:
            raise FileNotFoundError(f"No chunk files found matching: {pattern}")
        return chunks
```

**Usage in Pipeline**:

```python
class PipelineOrchestrator:
    def __init__(self, config: dict):
        self.config = config
        self.reading_mode = config['pipeline']['reading_mode']
    
    def process_file(self, input_file: str) -> dict:
        if self.reading_mode == 'multi':
            return self._process_multi_reader(input_file)
        else:
            return self._process_single_reader(input_file)
    
    def _process_multi_reader(self, input_file: str) -> dict:
        """Multi-reader mode - requires pre-split files."""
        multi_config = self.config['pipeline']['multi_reader']
        
        # Option 1: Use existing pre-split files
        if 'input_pattern' in multi_config:
            chunk_files = FileSplitValidator.find_existing_chunks(
                multi_config['input_pattern']
            )
        
        # Option 2: Split file first
        elif multi_config.get('split_before_processing'):
            splitter = FileSplitter(
                record_length=self.config['record_length'],
                target_chunk_size=multi_config.get('chunk_size_gb', 25) * 1024**3
            )
            chunk_files = splitter.split(
                input_file, 
                multi_config['split_output_dir']
            )
        
        else:
            raise ValueError(
                "Multi-reader mode requires either 'input_pattern' for "
                "existing chunks or 'split_before_processing: true'"
            )
        
        # Validate chunks
        FileSplitValidator.validate_chunks(
            chunk_files, 
            self.config['record_length']
        )
        
        # Now each reader gets its OWN file - no contention!
        return self._run_multi_reader_pipeline(chunk_files)
    
    def _run_multi_reader_pipeline(self, chunk_files: List[str]) -> dict:
        """Each reader processes its own chunk file."""
        batch_queue = mp.Queue(maxsize=QUEUE_MAX_SIZE)
        
        # One reader per chunk file
        reader_processes = []
        for i, chunk_file in enumerate(chunk_files):
            reader = ChunkFileReader(
                worker_id=i,
                file_path=chunk_file,  # Each reader has its OWN file
                record_length=self.config['record_length'],
                encoding=self.config['encoding'],
                output_queue=batch_queue,
                run_id=self.run_id
            )
            p = Process(target=reader.run)
            p.start()
            reader_processes.append(p)
        
        # ... start writers, wait for completion ...
```

## Why This Architecture?

| Benefit | Explanation |
|---------|-------------|
| **Hybrid Parallelism** | Multiprocessing for CPU-bound (parsing), threading for I/O-bound (DB) |
| **Backpressure Control** | Bounded queue prevents memory explosion if DB is slow |
| **True CPU Parallelism** | `multiprocessing` bypasses Python GIL for CPU-bound validation |
| **Lightweight I/O** | `threading` is lighter weight; GIL released during DB network I/O |
| **Memory Bounded** | Queue max size limits memory (10 batches × 5K = 50K records max) |
| **Separation of Concerns** | Reader/processor logic decoupled from DB writer logic |
| **Tunable Performance** | Adjust readers, processors, writers, queue size independently |
| **Fault Isolation** | One slow writer doesn't block others |
| **Shared Metrics** | Writer threads share memory for easy aggregation |

## Layer Responsibilities

### 1. IO Layer (`src/io_layer/`)
**Purpose**: Handle mainframe-specific file formats with chunked I/O
- EBCDIC encoding/decoding (cp037, cp500, cp1047)
- COBOL copybook parsing (fixed-width field extraction)
- Packed decimal (COMP-3) conversion
- **Chunked reading** (64KB buffer for 100x I/O efficiency)
- **File splitting** for multi-reader mode

**Key Classes**: `EBCDICReader`, `CopybookParser`, `FileSplitter`, `unpack_comp3()`

### 2. Core Layer (`src/core/`)
**Purpose**: Business logic (no I/O, no database dependencies)
- Domain models as `@dataclass`
- Validation rules (types, ranges, business constraints)
- Data transformations and enrichment
- Pure functions for testability
- **Stateless** - safe for parallel execution

**Key Classes**: `InputRecord`, `Validator`, `TransformationService`

### 3. Repository Layer (`src/repository/`)
**Purpose**: Data access for DB Writer processes
- **One connection per writer process** (not pooled across processes)
- Bulk insert operations (`executemany` with 5K batch)
- Transaction management per batch
- Retry logic with exponential backoff

**Key Classes**: `ConnectionFactory`, `Db2StagingRepository`, `Db2FinalRepository`, `RetryPolicy`

### 4. Pipeline Layer (`src/pipeline/`)
**Purpose**: Producer-consumer orchestration with hybrid parallelism
- **SingleReaderPipeline**: One reader → processor pool (multiprocessing) → writer pool (threading)
- **MultiReaderPipeline**: Multiple reader/processors (multiprocessing, one per chunk file) → writer pool (threading)
- Bounded queues for backpressure
- Queue adapter for multiprocessing → threading bridge
- Worker lifecycle management

**Key Classes**: `SingleReader`, `ChunkFileReader`, `Processor`, `DbWriter`, `PipelineOrchestrator`

### 5. Application Layer (`src/batch/`)
**Purpose**: Entry point and workflow
- Pipeline initialization and configuration
- **Reading mode selection** (single vs multi)
- Graceful shutdown handling (SIGTERM/SIGINT)
- Post-pipeline MERGE to final tables
- Reconciliation orchestration
- Exit code management

**Key Classes**: `BatchApplication`, `ShutdownHandler`

### 6. Utility Layer (`src/util/`)
**Purpose**: Cross-cutting concerns
- Configuration management (YAML + vault)
- **Per-worker structured logging** (JSON format with worker_id)
- Metrics collection and aggregation
- RUN_ID generation
- PII masking

**Key Classes**: `ConfigLoader`, `WorkerLogger`, `MetricsAggregator`, `PIIMasker`

## Core Design Principles

### 1. **Single Reader Pipeline (Default Mode)**

Best for: network storage, variable-length records, simplicity.

```python
import multiprocessing as mp
from multiprocessing import Process, Queue
from typing import List, Iterator
from dataclasses import dataclass
import queue

# Configuration constants
CHUNK_SIZE = 64 * 1024      # 64KB read buffer
BATCH_SIZE = 5000           # Records per batch
RAW_QUEUE_MAX = 20          # Reader → Processors queue
PROCESSED_QUEUE_MAX = 10    # Processors → Writers queue
NUM_PROCESSORS = 4          # Processing workers
NUM_WRITERS = 2             # DB writer workers


@dataclass
class RawBatch:
    """Batch of raw bytes/records for processing."""
    records: List[bytes]
    batch_num: int


@dataclass
class ProcessedBatch:
    """Batch of processed records ready for DB insert."""
    records: List[dict]
    worker_id: int
    batch_num: int


class SingleReader:
    """
    Single sequential file reader.
    
    Reads file in 64KB chunks, batches records, puts on queue
    for processor pool. No file contention issues.
    """
    
    def __init__(self, file_path: str, record_length: int,
                 encoding: str, output_queue: Queue):
        self.file_path = file_path
        self.record_length = record_length
        self.encoding = encoding
        self.output_queue = output_queue
        self.records_read = 0
        self.batches_produced = 0
    
    def run(self) -> dict:
        """Read entire file sequentially, batch and queue."""
        batch = []
        buffer = b''
        
        with open(self.file_path, 'rb') as f:
            while True:
                chunk = f.read(CHUNK_SIZE)
                if not chunk:
                    break
                
                buffer += chunk
                
                # Extract complete records from buffer
                while len(buffer) >= self.record_length:
                    record = buffer[:self.record_length]
                    buffer = buffer[self.record_length:]
                    batch.append(record)
                    self.records_read += 1
                    
                    if len(batch) >= BATCH_SIZE:
                        self._queue_batch(batch)
                        batch = []
        
        # Final partial batch
        if batch:
            self._queue_batch(batch)
        
        return {
            'records_read': self.records_read,
            'batches_produced': self.batches_produced
        }
    
    def _queue_batch(self, batch: List[bytes]) -> None:
        """Put batch on queue (blocks if full - backpressure)."""
        raw_batch = RawBatch(
            records=batch.copy(),
            batch_num=self.batches_produced
        )
        self.output_queue.put(raw_batch)
        self.batches_produced += 1


class Processor:
    """
    Batch processor worker.
    
    Takes raw batches from reader queue, parses/validates/transforms,
    puts processed batches on writer queue.
    """
    
    def __init__(self, worker_id: int, input_queue: Queue,
                 output_queue: Queue, encoding: str,
                 copybook_fields: List, run_id: str):
        self.worker_id = worker_id
        self.input_queue = input_queue
        self.output_queue = output_queue
        self.encoding = encoding
        self.copybook_fields = copybook_fields
        self.run_id = run_id
        self.batches_processed = 0
        self.records_valid = 0
        self.records_invalid = 0
    
    def run(self) -> dict:
        """Process batches until poison pill received."""
        parser = CopybookParser(self.copybook_fields)
        validator = Validator()
        
        while True:
            try:
                raw_batch = self.input_queue.get(timeout=30)
                
                if raw_batch is None:  # Poison pill
                    break
                
                processed_records = []
                
                for raw_record in raw_batch.records:
                    # Decode and parse
                    decoded = raw_record.decode(self.encoding)
                    parsed = parser.parse(raw_record)
                    
                    # Validate
                    errors = validator.validate(parsed)
                    if errors:
                        self.records_invalid += 1
                        continue
                    
                    processed_records.append(parsed)
                    self.records_valid += 1
                
                # Queue for writers
                if processed_records:
                    self.output_queue.put(ProcessedBatch(
                        records=processed_records,
                        worker_id=self.worker_id,
                        batch_num=raw_batch.batch_num
                    ))
                
                self.batches_processed += 1
                
            except queue.Empty:
                continue
        
        return {
            'worker_id': self.worker_id,
            'batches_processed': self.batches_processed,
            'records_valid': self.records_valid,
            'records_invalid': self.records_invalid
        }


class SingleReaderPipeline:
    """
    Orchestrates single-reader pipeline with hybrid parallelism.
    
    Architecture:
      SingleReader (Main) → RawQueue → Processors (N processes) → ProcessedQueue → Writers (M threads)
    
    Why hybrid?
      - Processors use multiprocessing: CPU-bound work (GIL blocks threading)
      - Writers use threading: I/O-bound work (GIL released during DB network I/O)
    
    Queue Bridge Pattern:
      - Processors write to multiprocessing.Manager().Queue() (cross-process safe)
      - Adapter thread transfers to queue.Queue() (for writer threads)
      - This bridges the process/thread boundary efficiently
    """
    
    def __init__(self, config: dict):
        self.config = config
        self.run_id = self._generate_run_id()
    
    def process_file(self, file_path: str) -> dict:
        """Process file using single-reader pipeline."""
        
        # Create queues
        # multiprocessing.Queue for reader → processors (cross-process)
        raw_queue = mp.Queue(maxsize=RAW_QUEUE_MAX)
        
        # Manager Queue for processors → adapter (cross-process safe)
        manager = mp.Manager()
        mp_processed_queue = manager.Queue(maxsize=PROCESSED_QUEUE_MAX)
        
        # Thread Queue for adapter → writers (in-process, thread-safe)
        from queue import Queue as ThreadQueue
        writer_queue = ThreadQueue(maxsize=PROCESSED_QUEUE_MAX)
        
        # Queue adapter thread: bridges multiprocessing → threading
        def queue_adapter():
            """Transfer batches from multiprocessing queue to thread queue."""
            while True:
                batch = mp_processed_queue.get()
                if batch is None:
                    break
                writer_queue.put(batch)
        
        adapter_thread = threading.Thread(target=queue_adapter, daemon=True)
        adapter_thread.start()
        
        # Start processor workers (multiprocessing - CPU-bound)
        processors = []
        for i in range(NUM_PROCESSORS):
            proc = Processor(
                worker_id=i,
                input_queue=raw_queue,
                output_queue=mp_processed_queue,  # Manager Queue (cross-process)
                encoding=self.config['encoding'],
                copybook_fields=self.config['copybook_fields'],
                run_id=self.run_id
            )
            p = Process(target=proc.run)
            p.start()
            processors.append(p)
        
        # Start DB writers (threading - I/O-bound)
        writers = []
        for i in range(NUM_WRITERS):
            writer = DbWriter(
                writer_id=i,
                input_queue=writer_queue,  # Thread Queue
                db_config=self.config['db'],
                run_id=self.run_id
            )
            writer.start()  # Thread.start()
            writers.append(writer)
        
        # Run single reader (in main process)
        reader = SingleReader(
            file_path=file_path,
            record_length=self.config['record_length'],
            encoding=self.config['encoding'],
            output_queue=raw_queue
        )
        reader_result = reader.run()
        
        # Signal processors to stop
        for _ in processors:
            raw_queue.put(None)
        for p in processors:
            p.join()
        
        # Signal queue adapter to stop
        mp_processed_queue.put(None)
        adapter_thread.join()
        
        # Signal writers to stop (poison pills)
        for _ in writers:
            writer_queue.put(None)
        
        # Wait for writer threads
        for w in writers:
            w.join()
        
        # Aggregate writer metrics
        writer_results = [w.get_result() for w in writers]
        total_written = sum(r['records_written'] for r in writer_results)
        
        return {
            'run_id': self.run_id,
            'reader': reader_result,
            'writers': writer_results,
            'total_written': total_written,
            'status': 'complete'
        }
    
    def _generate_run_id(self) -> str:
        from datetime import datetime
        return datetime.now().strftime('%Y%m%d_%H%M%S')
```

### 2. **Multi-Reader Pipeline (Pre-Split Files)**

Best for: local SSD, fixed-length records, maximum parallelism.

**Requirement**: Files MUST be pre-split before processing. Each reader gets its own file.

```python
class ChunkFileReader:
    """
    Reader for pre-split chunk files.
    
    Each reader instance processes ONE chunk file exclusively.
    No file contention - each reader has its own file.
    """
    
    def __init__(self, worker_id: int, file_path: str,
                 record_length: int, encoding: str,
                 output_queue: Queue, copybook_fields: List,
                 run_id: str):
        self.worker_id = worker_id
        self.file_path = file_path  # This reader's EXCLUSIVE file
        self.record_length = record_length
        self.encoding = encoding
        self.output_queue = output_queue
        self.copybook_fields = copybook_fields
        self.run_id = run_id
        
        # Metrics
        self.records_read = 0
        self.records_valid = 0
        self.records_invalid = 0
        self.batches_produced = 0
    
    def run(self) -> dict:
        """Process chunk file - called in subprocess."""
        parser = CopybookParser(self.copybook_fields)
        validator = Validator()
        
        batch = []
        buffer = b''
        
        # Each reader opens its OWN file - no contention!
        with open(self.file_path, 'rb') as f:
            while True:
                chunk = f.read(CHUNK_SIZE)
                if not chunk:
                    break
                
                buffer += chunk
                
                while len(buffer) >= self.record_length:
                    raw_record = buffer[:self.record_length]
                    buffer = buffer[self.record_length:]
                    
                    self.records_read += 1
                    
                    # Decode, parse, validate
                    parsed = parser.parse(raw_record)
                    errors = validator.validate(parsed)
                    
                    if errors:
                        self.records_invalid += 1
                        continue
                    
                    self.records_valid += 1
                    batch.append(parsed)
                    
                    if len(batch) >= BATCH_SIZE:
                        self._send_batch(batch)
                        batch = []
        
        if batch:
            self._send_batch(batch)
        
        return {
            'worker_id': self.worker_id,
            'file': self.file_path,
            'records_read': self.records_read,
            'records_valid': self.records_valid,
            'records_invalid': self.records_invalid,
            'batches_produced': self.batches_produced
        }
    
    def _send_batch(self, batch: List) -> None:
        processed_batch = ProcessedBatch(
            records=batch.copy(),
            worker_id=self.worker_id,
            batch_num=self.batches_produced
        )
        self.output_queue.put(processed_batch)
        self.batches_produced += 1


class MultiReaderPipeline:
    """
    Orchestrates multi-reader pipeline with pre-split files.
    
    Architecture:
      chunk_001.dat → Reader/Processor 1 (Process) ─┐
      chunk_002.dat → Reader/Processor 2 (Process) ─┼→ Queue → Writers (M Threads)
      chunk_003.dat → Reader/Processor 3 (Process) ─┘
    
    Why hybrid?
      - Readers use multiprocessing: CPU-bound parse/validate, separate files
      - Writers use threading: I/O-bound DB writes, GIL released during network I/O
    """
    
    def __init__(self, config: dict):
        self.config = config
        self.run_id = self._generate_run_id()
    
    def process_chunks(self, chunk_files: List[str]) -> dict:
        """Process pre-split chunk files in parallel."""
        
        # Validate all chunks exist and are aligned
        FileSplitValidator.validate_chunks(
            chunk_files, 
            self.config['record_length']
        )
        
        # Thread-safe queue for writer threads
        from queue import Queue as ThreadQueue
        batch_queue = ThreadQueue(maxsize=QUEUE_MAX_SIZE)
        
        # Manager for cross-process → thread queue communication
        manager = mp.Manager()
        mp_to_thread_queue = manager.Queue(maxsize=QUEUE_MAX_SIZE)
        
        # One reader per chunk file - NO CONTENTION (multiprocessing)
        readers = []
        for i, chunk_file in enumerate(chunk_files):
            reader = ChunkFileReader(
                worker_id=i,
                file_path=chunk_file,
                record_length=self.config['record_length'],
                encoding=self.config['encoding'],
                output_queue=mp_to_thread_queue,  # Cross-process queue
                copybook_fields=self.config['copybook_fields'],
                run_id=self.run_id
            )
            p = Process(target=reader.run)
            p.start()
            readers.append(p)
        
        # Queue adapter thread: moves from mp.Queue to thread Queue
        def queue_adapter():
            """Transfer batches from multiprocessing queue to thread queue."""
            while True:
                batch = mp_to_thread_queue.get()
                if batch is None:
                    break
                batch_queue.put(batch)
        
        adapter_thread = threading.Thread(target=queue_adapter, daemon=True)
        adapter_thread.start()
        
        # Start DB writers (threading - I/O-bound)
        writers = []
        for i in range(NUM_WRITERS):
            writer = DbWriter(
                writer_id=i,
                input_queue=batch_queue,
                db_config=self.config['db'],
                run_id=self.run_id
            )
            writer.start()  # Thread.start()
            writers.append(writer)
        
        # Wait for all readers (processes)
        for p in readers:
            p.join()
        
        # Signal queue adapter to stop
        mp_to_thread_queue.put(None)
        adapter_thread.join()
        
        # Signal writers to stop (poison pills)
        for _ in writers:
            batch_queue.put(None)
        
        # Wait for writer threads
        for w in writers:
            w.join()
        
        # Aggregate writer metrics
        writer_results = [w.get_result() for w in writers]
        total_written = sum(r['records_written'] for r in writer_results)
        
        return {
            'run_id': self.run_id,
            'writers': writer_results,
            'total_written': total_written,
            'status': 'complete'
        }
    
    def _generate_run_id(self) -> str:
        from datetime import datetime
        return datetime.now().strftime('%Y%m%d_%H%M%S')
```

### 3. **DbWriter (Shared by Both Pipelines - Threading)**

```python
import threading
from queue import Queue as ThreadQueue  # Thread-safe queue for writers


class DbWriter(threading.Thread):
    """
    DB Writer worker - runs in separate THREAD.
    
    Why threading (not multiprocessing)?
    - DB I/O releases the GIL during network operations
    - Threads are lighter weight than processes
    - Shared memory makes metrics aggregation easier
    - No IPC overhead for queue communication
    
    Responsibilities:
    - Own dedicated DB connection (not shared)
    - Take batches from queue
    - Bulk insert to staging table
    - Retry on transient failures
    """
    
    def __init__(self, writer_id: int, input_queue: ThreadQueue,
                 db_config: dict, run_id: str):
        super().__init__(name=f"DbWriter-{writer_id}", daemon=True)
        self.writer_id = writer_id
        self.input_queue = input_queue
        self.db_config = db_config
        self.run_id = run_id
        
        # Metrics (thread-safe via GIL for simple increments)
        self.batches_written = 0
        self.records_written = 0
        self._result = None
    
    def run(self) -> None:
        """Main writing loop - called in thread."""
        # Each writer has its OWN connection (not shared)
        conn = self._create_connection()
        repository = Db2StagingRepository(conn)
        
        try:
            while True:
                try:
                    # Wait for batch from queue (timeout for graceful shutdown)
                    batch = self.input_queue.get(timeout=30)
                    
                    if batch is None:  # Poison pill - shutdown signal
                        break
                    
                    # Bulk insert with retry
                    self._insert_with_retry(repository, batch)
                    
                    # Mark task done for join() support
                    self.input_queue.task_done()
                    
                except queue.Empty:
                    # Timeout - check if should continue
                    continue
        finally:
            conn.close()
        
        self._result = {
            'writer_id': self.writer_id,
            'batches_written': self.batches_written,
            'records_written': self.records_written
        }
    
    def get_result(self) -> dict:
        """Get metrics after thread completes."""
        return self._result
    
    def _insert_with_retry(self, repository, batch: ProcessedBatch, 
                           max_attempts: int = 3) -> None:
        """Insert batch with exponential backoff retry."""
        for attempt in range(1, max_attempts + 1):
            try:
                repository.bulk_insert(
                    batch.records, 
                    self.run_id,
                    worker_id=self.writer_id
                )
                self.batches_written += 1
                self.records_written += len(batch.records)
                return
                
            except Exception as e:
                if attempt == max_attempts:
                    raise
                delay = 2 ** (attempt - 1)  # 1s, 2s, 4s
                time.sleep(delay)
    
    def _create_connection(self):
        """Create dedicated DB connection for this writer thread."""
        return ibm_db_dbi.connect(
            self.db_config['connection_string'],
            self.db_config.get('user'),
            self.db_config.get('password')
        )
```

### 4. **PipelineOrchestrator (Mode Selection)**

```python
class PipelineOrchestrator:
    """
    Main orchestrator - selects pipeline mode based on configuration.
    
    Modes:
    - 'single': SingleReaderPipeline (default, recommended)
    - 'multi': MultiReaderPipeline (requires pre-split files)
    """
    
    def __init__(self, config: dict):
        self.config = config
        self.reading_mode = config.get('pipeline', {}).get('reading_mode', 'single')
    
    def process_file(self, input_file: str) -> dict:
        """
        Process file using configured pipeline mode.
        
        Args:
            input_file: Path to input file (or pattern for multi mode)
        
        Returns:
            Aggregated metrics from pipeline
        """
        if self.reading_mode == 'multi':
            return self._process_multi_mode(input_file)
        else:
            return self._process_single_mode(input_file)
    
    def _process_single_mode(self, input_file: str) -> dict:
        """Single reader mode - recommended for most cases."""
        pipeline = SingleReaderPipeline(self.config)
        return pipeline.process_file(input_file)
    
    def _process_multi_mode(self, input_file: str) -> dict:
        """
        Multi-reader mode - requires pre-split files.
        
        Configuration options:
        1. input_pattern: Glob pattern for existing chunks
        2. split_before_processing: Split file first, then process
        """
        multi_config = self.config.get('pipeline', {}).get('multi_reader', {})
        
        # Option 1: Use existing pre-split chunk files
        if 'input_pattern' in multi_config:
            chunk_files = FileSplitValidator.find_existing_chunks(
                multi_config['input_pattern']
            )
        
        # Option 2: Split the file first, then process
        elif multi_config.get('split_before_processing', False):
            splitter = FileSplitter(
                record_length=self.config['record_length'],
                target_chunk_size=multi_config.get('chunk_size_gb', 25) * 1024**3
            )
            chunk_files = splitter.split(
                input_file,
                multi_config.get('split_output_dir', '/tmp/chunks')
            )
        
        else:
            raise ValueError(
                "Multi-reader mode requires either:\n"
                "  - 'input_pattern' for existing chunk files, OR\n"
                "  - 'split_before_processing: true' with 'split_output_dir'"
            )
        
        # Process using multi-reader pipeline
        pipeline = MultiReaderPipeline(self.config)
        return pipeline.process_chunks(chunk_files)
```

### 5. **Pipeline Tuning Guidelines**

| Parameter | Default | Adjust When |
|-----------|---------|-------------|
| `NUM_PROCESSORS` | 4 | CPU < 70% → increase; CPU > 90% → decrease |
| `NUM_WRITERS` | 2 | Queue growing → increase; DB overloaded → decrease |
| `QUEUE_MAX_SIZE` | 10 | Memory high → decrease; Writers starving → increase |
| `BATCH_SIZE` | 5,000 | DB slow → try 10K; DB locks → try 2K |
| `CHUNK_SIZE` | 64KB | Rarely needs adjustment |

**Why Threading for Writers?**
- DB I/O is network-bound, not CPU-bound
- Python GIL is released during network operations
- Threads have lower overhead than processes (no IPC)
- Shared memory allows easy metrics aggregation

**Resource Monitoring**:
```python
# Log queue depth periodically for tuning
def monitor_queue(queue: Queue, interval: int = 10):
    while True:
        depth = queue.qsize()
        logger.info(f"Queue depth: {depth}/{QUEUE_MAX_SIZE}")
        if depth >= QUEUE_MAX_SIZE * 0.8:
            logger.warning("Queue near capacity - writers may be slow")
        time.sleep(interval)
```

### 3. **Memory Budget (Pipeline Mode)**

| Component | Per Reader | Per Writer | Total (4R + 2W) |
|-----------|-----------|------------|-----------------|
| Chunk buffer | 64KB | - | 256KB |
| Record buffer | 64KB | - | 256KB |
| Current batch | 50MB | - | 200MB |
| Queue batches | - | - | 250MB (10 × 25MB) |
| DB connection | - | 5MB | 10MB |
| **Worker Total** | ~55MB | ~5MB | **~500MB** |

### 4. **Batch Database Operations**
```python
# GOOD: Batch inserts
cursor.executemany(sql, batch_of_5000_records)

# BAD: Individual inserts
for record in records:
    cursor.execute(sql, record)  # ❌ 100x slower
```

### 5. **RUN_ID + WORKER_ID Traceability**
```python
run_id = f"{datetime.now().strftime('%Y%m%d_%H%M%S')}"
# Every staging record tagged with RUN_ID + WORKER_ID
# Enables: debugging, restartability, per-worker reconciliation
```

### 6. **Staging-First Pattern**
```python
# Step 1: Pipeline loads to staging (parallel, fast)
orchestrator.process_file(file_path)

# Step 2: Merge to final (single process, after pipeline completes)
final_repo.merge_from_staging(run_id)

# Step 3: Reconcile counts (aggregate from all workers)
reconciliation_service.verify(run_id, source_count)
```

## Pipeline Data Flow

```python
# Complete pipeline usage
def main():
    config = ConfigLoader().load()
    
    # Initialize pipeline
    orchestrator = PipelineOrchestrator(config)
    
    # Process file through pipeline
    result = orchestrator.process_file("/data/mainframe_100gb.dat")
    run_id = result['run_id']
    
    # Post-pipeline: Merge to final (single process)
    final_repo = Db2FinalRepository(config['db'])
    final_repo.merge_from_staging(run_id)
    
    # Reconciliation
    recon = ReconciliationService(config['db'])
    recon.verify_counts(run_id)
    
    logger.info(f"Pipeline complete: {run_id}")


if __name__ == '__main__':
    main()
```

## When to Use This Architecture

✅ **Use when:**
- Processing mainframe EBCDIC files with COBOL layouts
- Processing ASCII/UTF-8 fixed-width or delimited files
- Need high throughput (10K+ records/second)
- Require audit trail and data lineage (RUN_ID)
- Must support restartability and idempotency
- Database is DB2 (z/OS or LUW)
- Deployment is web farm (non-cloud)

🔄 **Adjust for ASCII/UTF-8 files:**
- IO layer still provides: streaming, parsing abstraction, field mapping
- Change encoding parameter: `encoding='utf-8'` instead of `cp037`
- Skip `unpack_comp3()` if no packed decimals
- Use CSV/delimited parser if not fixed-width

```python
# ASCII/UTF-8 fixed-width file
class ASCIIReader:
    def __init__(self, file_path: str, encoding: str = 'utf-8', line_length: int = None):
        self._file_path = file_path
        self._encoding = encoding
        self._line_length = line_length
    
    def read_lines(self) -> Iterator[str]:
        with open(self._file_path, 'r', encoding=self._encoding) as f:
            if self._line_length:
                while True:
                    line = f.read(self._line_length)
                    if not line:
                        break
                    yield line.rstrip('\n\r')
            else:
                for line in f:
                    yield line.rstrip('\n\r')

# CSV/Delimited file
class DelimitedReader:
    def __init__(self, file_path: str, delimiter: str = ',', encoding: str = 'utf-8'):
        self._file_path = file_path
        self._delimiter = delimiter
        self._encoding = encoding
    
    def read_records(self) -> Iterator[Dict[str, str]]:
        import csv
        with open(self._file_path, 'r', encoding=self._encoding, newline='') as f:
            reader = csv.DictReader(f, delimiter=self._delimiter)
            for row in reader:
                yield row
```

❌ **Don't use when:**
- Real-time/streaming requirements (use Kafka/messaging)
- NoSQL data store (adjust repository layer)
- Cloud-native deployment (consider serverless alternatives)

## Project Structure Template

```
batch_project/
├── config/
│   ├── base_config.yaml              # Shared settings (batch size, workers, etc.)
│   ├── dev_config.yaml               # Development environment overrides
│   ├── test_config.yaml              # Test environment overrides
│   ├── prod_config.yaml              # Production environment overrides
│   └── secrets/                      # .gitignored - vault-managed
│       └── {env}_secrets.yaml        # DB credentials, API keys
│
├── src/
│   ├── core/                         # Business logic (NO I/O, NO DB)
│   │   ├── __init__.py
│   │   ├── models.py                 # Domain models (@dataclass)
│   │   ├── validators.py             # Business rules validation
│   │   └── services/
│   │       ├── __init__.py
│   │       ├── transformation_service.py
│   │       └── reconciliation_service.py
│   │
│   ├── io_layer/                     # File I/O (EBCDIC, copybooks, CSV)
│   │   ├── __init__.py
│   │   ├── ebcdic_reader.py          # Chunked EBCDIC reading
│   │   ├── ascii_reader.py           # ASCII/UTF-8 file reading
│   │   ├── delimited_reader.py       # CSV/delimited file reading
│   │   ├── copybook_parser.py        # Fixed-width field extraction
│   │   ├── packed_decimal.py         # COMP-3 conversion
│   │   ├── file_partitioner.py       # Split files for parallel processing
│   │   └── file_writer.py            # Output/error file writing
│   │
│   ├── pipeline/                     # Producer-Consumer Pipeline
│   │   ├── __init__.py
│   │   ├── orchestrator.py           # PipelineOrchestrator - main controller
│   │   ├── reader_processor.py       # ReaderProcessor worker
│   │   ├── db_writer.py              # DbWriter worker
│   │   ├── bounded_queue.py          # Queue with backpressure
│   │   └── worker_metrics.py         # Per-worker metrics collection
│   │
│   ├── repository/                   # DB2 access layer
│   │   ├── __init__.py
│   │   ├── connection_factory.py     # Connection creation (per-worker)
│   │   ├── staging_repository.py     # Staging table bulk insert
│   │   ├── final_repository.py       # Final table MERGE operations
│   │   ├── reconciliation_repository.py
│   │   └── retry_policy.py           # Exponential backoff decorator
│   │
│   ├── batch/                        # Application entry point
│   │   ├── __init__.py
│   │   ├── app.py                    # Main entry, post-pipeline MERGE
│   │   └── shutdown_handler.py       # Graceful shutdown (SIGTERM/SIGINT)
│   │
│   └── util/                         # Cross-cutting concerns
│       ├── __init__.py
│       ├── config_loader.py          # YAML + vault configuration
│       ├── worker_logger.py          # Per-worker structured logging
│       ├── metrics_aggregator.py     # Aggregate metrics from workers
│       ├── run_id.py                 # RUN_ID generation
│       └── pii_masker.py             # PII masking utilities
│
├── tests/
│   ├── unit/                         # Fast, isolated tests (70% coverage)
│   │   ├── core/
│   │   ├── io_layer/
│   │   ├── pipeline/
│   │   └── repository/
│   ├── integration/                  # DB2 integration tests (20% coverage)
│   │   └── test_pipeline_e2e.py
│   └── fixtures/                     # Test data and mocks
│       ├── sample_ebcdic.dat
│       ├── sample_copybook.cpy
│       └── mock_queue.py
│
├── scripts/
│   ├── run_batch.py                  # CLI wrapper
│   └── reconciliation/
│       └── aggregate_counts.py       # Aggregate worker counts
│
├── requirements.txt
├── requirements-dev.txt
└── README.md
```

### Layer Rules

| Layer | Can Import | Cannot Import |
|-------|------------|---------------|
| `core/` | Standard library, `util/` | `io_layer/`, `pipeline/`, `repository/`, `batch/` |
| `io_layer/` | Standard library, `util/` | `core/`, `pipeline/`, `repository/`, `batch/` |
| `pipeline/` | `core/`, `io_layer/`, `repository/`, `util/` | `batch/` |
| `repository/` | Standard library, `util/`, `core/models` | `io_layer/`, `pipeline/`, `batch/` |
| `batch/` | All layers | - |
| `util/` | Standard library only | All other layers |

### Why This Structure?

1. **Testability**: Core logic has no external dependencies → easy unit testing
2. **Pipeline Isolation**: Pipeline orchestration separate from business logic
3. **Flexibility**: Swap file formats (EBCDIC ↔ CSV) without changing pipeline
4. **Parallelism**: Workers are stateless → safe for multiprocessing
5. **Backpressure**: Bounded queue prevents memory explosion

## Quick Reference

| Concern | Implementation |
|---------|----------------|
| **File Partitioning** | `FilePartitioner` splits by byte range |
| **Chunk Reading** | `EBCDICReader` with 64KB buffer |
| **Parallel Processing** | `ReaderProcessor` workers (multiprocessing) |
| **Backpressure** | `BoundedQueue` (max 10 batches) |
| **DB Writing** | `DbWriter` workers with own connections |
| **COBOL Layouts** | `CopybookParser` with field definitions |
| **Packed Decimals** | `unpack_comp3()` utility |
| **Domain Models** | `@dataclass` with validation |
| **DB Operations** | `executemany` with 5K batch size |
| **Connections** | One per `DbWriter` (not pooled across processes) |
| **Retry Logic** | `@RetryPolicy(max_attempts=3)` in DbWriter |
| **Traceability** | `RUN_ID + WORKER_ID` in staging tables |
| **Idempotency** | `MERGE` statements in final tables |
| **Reconciliation** | Aggregate counts from all workers |
| **Logging** | Per-worker JSON with `worker_id` |
| **Config** | YAML with vault for secrets |

---

## Requirements.txt Template

When generating Python code, **always include/update requirements.txt** with all dependencies.

### requirements.txt (Production)
```text
# Python 3.12.3 required
# Core - Standard Library extensions
python-dateutil>=2.8.0

# DB2 Database
ibm-db>=3.1.0
ibm-db-dbi>=3.0.0

# Configuration
pyyaml>=6.0
jsonschema>=4.0.0

# Secrets Management (if using vault)
hvac>=1.0.0

# Encryption
cryptography>=41.0.0
```

### requirements-dev.txt (Development/Testing)
```text
# Testing
pytest>=7.4.0
pytest-cov>=4.1.0
pytest-mock>=3.11.0

# Code Quality
black>=23.0.0
isort>=5.12.0
mypy>=1.5.0
```

### Key Rules:
1. **Pin with `>=`** - Allow compatible updates (e.g., `ibm-db>=3.1.0`)
2. **Group by purpose** - Use comments to organize
3. **Scan all imports** - Every `import X` needs a requirements entry
4. **Separate dev deps** - Keep test/dev tools in requirements-dev.txt

---

## Common Pitfalls

❌ **Loading entire file into memory**
```python
# Bad
data = open(file).read()  # OOM on 1GB+ files
```

✅ **Stream line-by-line**
```python
# Good
for line in reader.read_lines():  # Constant memory
    process(line)
```

❌ **Individual DB inserts**
```python
# Bad - 100x slower
for rec in records:
    cursor.execute(sql, rec)
```

✅ **Batch inserts**
```python
# Good - 100x faster
cursor.executemany(sql, batch)
```

❌ **Using float for money**
```python
# Bad - precision loss
amount = float(123.45)  # Can become 123.44999998
```

✅ **Using Decimal**
```python
# Good - exact precision
amount = Decimal('123.45')
```

❌ **Assuming ASCII encoding**
```python
# Bad
text = bytes.decode('utf-8')  # Fails on EBCDIC
```

✅ **Explicit EBCDIC handling**
```python
# Good
text = bytes.decode('cp037')  # EBCDIC code page
```

## Next Steps

After understanding the architecture:
1. **File Processing**: See `01-ebcdic-copybook-handling.md`
2. **Data Access**: See `02-repository-pattern.md`
3. **Configuration**: See `03-configuration-management.md`
4. **Quality**: See `04-reconciliation.md`



# Phase 6: Pipeline Orchestrator

> **Use Case**: Implement the hybrid producer-consumer pipeline with multiprocessing for CPU-bound work and threading for I/O-bound DB writes.

---

## Prompt

```
@workspace For the [JOB_NAME] modernization, implement the hybrid pipeline:

1. PipelineOrchestrator class with hybrid parallelism:
   - Processors: multiprocessing.Process (CPU-bound parsing/validation)
   - Writers: threading.Thread (I/O-bound DB writes, GIL released)
2. Queue bridge pattern:
   - mp.Manager().Queue() for cross-process communication
   - Adapter thread transfers to queue.Queue() for writer threads
3. SingleReader in main thread → raw_queue → Processors
4. Processors → mp_processed_queue → adapter → writer_queue → DbWriters
5. DbWriter threads: each with own DB connection → bulk insert to staging
6. Bounded queues (20 raw, 10 processed) for backpressure control
7. Graceful shutdown: poison pills for processors, then adapter, then writers
8. RUN_ID + WORKER_ID for traceability
9. WorkerLogger with process_name for processors, thread_name for writers

Memory budget: < 500MB total regardless of file size.
Generate python/[job_name]/src/processor/ and src/batch/app.py
```

---

## Expected Files

```
python/[job_name]/src/
├── processor/
│   ├── __init__.py
│   ├── orchestrator.py        # Main pipeline coordinator
│   ├── single_reader.py       # Sequential file reader
│   ├── processor.py           # multiprocessing.Process workers
│   ├── queue_adapter.py       # mp.Queue → queue.Queue bridge
│   ├── db_writer.py           # threading.Thread DB writers
│   ├── validators.py          # Validation rules
│   └── transformers.py        # Data transformations
├── batch/
│   ├── __init__.py
│   ├── app.py                 # Entry point
│   └── shutdown_handler.py    # Graceful SIGTERM handling
└── utility/
    └── worker_logger.py       # Per-worker logging
```

---

## Architecture Flow

```
┌─────────────────┐
│  SINGLE READER  │ (Main Thread)
│  64KB chunks    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ RAW BATCHES     │ (mp.Queue, max=20)
│ QUEUE           │
└────────┬────────┘
         │
    ┌────┴────┬────────┐
    ▼         ▼        ▼
┌────────┐ ┌────────┐ ┌────────┐
│PROC 1  │ │PROC 2  │ │PROC N  │ (multiprocessing.Process)
│Parse   │ │Parse   │ │Parse   │
│Validate│ │Validate│ │Validate│
└───┬────┘ └───┬────┘ └───┬────┘
    │          │          │
    └────┬─────┴────┬─────┘
         ▼          
┌─────────────────┐
│ MP MANAGER      │ (Manager().Queue, max=10)
│ QUEUE           │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ QUEUE ADAPTER   │ (Bridge Thread)
│ mp → threading  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ WRITER QUEUE    │ (queue.Queue, max=10)
└────────┬────────┘
         │
    ┌────┴────┬────────┐
    ▼         ▼        ▼
┌────────┐ ┌────────┐ ┌────────┐
│WRITER 1│ │WRITER 2│ │WRITER M│ (threading.Thread)
│Own conn│ │Own conn│ │Own conn│
│Bulk ins│ │Bulk ins│ │Bulk ins│
└────────┘ └────────┘ └────────┘
```

---

## Prerequisites
- All previous prompts complete (02 through 05)
- Configuration files ready

---

## Next Steps
Proceed to `07-reconciliation.prompt.md` for audit checks

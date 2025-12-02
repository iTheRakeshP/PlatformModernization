# Modernization Prompt Templates

Ready-to-use prompts for modernizing mainframe COBOL batch jobs to Python. Copy and customize these prompts for your projects.

> **Python Version**: **3.12.3** required. Use `match` statements, type hints with `|` union, and modern Python idioms.

> **⚠️ Critical**: Input files can exceed **100GB+**. All generated code MUST use the **hybrid producer-consumer pipeline**: multiprocessing for CPU-bound parsing, threading for I/O-bound DB writes. See `00-core-architecture.md` for the full pattern.

---

## Quick Start

### 1. Full Job Modernization (Recommended)

```
@workspace Modernize the entire [JOB_NAME] batch job in source/jobs/[JOB_NAME]/.

Please:
1. Analyze the JCL to understand the complete job flow
2. Review all COBOL programs and copybooks
3. Create a modernization plan showing COBOL → Python mapping
4. Generate the complete Python project using our hybrid producer-consumer pipeline:
   - Domain models from copybooks
   - IO layer with chunked EBCDIC reading (64KB buffer)
   - Pipeline layer with hybrid parallelism:
     * Processors (multiprocessing) for CPU-bound parsing/validation
     * DbWriters (threading) for I/O-bound DB writes
     * Queue bridge: mp.Manager().Queue() → adapter thread → queue.Queue()
   - Repository layer for DB2 staging/final (one connection per writer thread)
   - Configuration files with pipeline tuning params
   - Reconciliation checks aggregating from all workers
   - WorkerLogger for per-worker logging (process_name or thread_name)
   - requirements.txt with all dependencies (pinned versions)
   - requirements-dev.txt for test dependencies

Memory budget: < 500MB regardless of file size.
Output the Python project to python/[job_name]/
```

### 2. Simple Version (Trust the Instructions)

```
@workspace Modernize source/jobs/[JOB_NAME]/ to Python.
Use producer-consumer pipeline for 100GB+ files.
Follow our copilot-instructions architecture. Generate complete project in python/[job_name]/.
```

---

## Phase-by-Phase Approach

Use this when you want more control over the modernization process.

### Phase 1: Analysis & Planning

```
@workspace Analyze the batch job in source/jobs/[JOB_NAME]/.

Please provide:
1. JCL analysis - list all steps, programs, input/output files, DB2 tables
2. Data flow diagram - how data moves through the job
3. Copybook summary - all record layouts with field types
4. Modernization plan - how COBOL components map to Python layers
5. Estimated complexity and any special considerations

Do not generate code yet, just the analysis.
```

### Phase 2: Project Structure & Domain Models

```
@workspace Based on the analysis of [JOB_NAME], create:

1. Python project structure in python/[job_name]/ per 00-core-architecture.md
   - Include pipeline/ folder for producer-consumer pattern
2. Domain models (@dataclass) from all copybooks in source/jobs/[JOB_NAME]/copybooks/
3. Handle all COMP-3 packed decimal fields properly
4. Include validation rules based on field definitions

Generate the src/core/models.py and src/core/validators.py files.
```

### Phase 3: IO Layer (with File Partitioning)

```
@workspace For the [JOB_NAME] modernization, implement the IO layer:

1. FilePartitioner for splitting 100GB+ files by byte ranges
2. PartitionedEBCDICReader with 64KB chunked reading
3. CopybookParser with field definitions from copybooks
4. File writers for any output files
5. Handle all data types: PIC X, PIC 9, COMP-3, COMP

Follow patterns from 01-ebcdic-copybook-handling.md.
Generate files in python/[job_name]/src/io_layer/
```

### Phase 4: Repository Layer (DbWriter Pattern)

```
@workspace For the [JOB_NAME] modernization, implement the repository layer:

1. ConnectionFactory (NOT connection pool - each DbWriter thread gets own connection)
2. DbWriter class extending threading.Thread with:
   - Own DB connection created in run()
   - bulk_insert using executemany (5K records per batch)
   - get_result() method for metrics after join()
3. StagingRepository with WORKER_ID column for traceability
4. FinalRepository with MERGE operations (post-pipeline)
5. ReconciliationRepository for audit trails
6. RetryPolicy decorator with exponential backoff for transient failures

Follow patterns from 02-repository-pattern.md.
Generate files in python/[job_name]/src/repository/
```

### Phase 5: Configuration (with Pipeline Tuning)

```
@workspace For the [JOB_NAME] modernization, create configuration:

1. base_config.yaml with hybrid pipeline settings:
   - reading_mode: "single"  # or "multi" for pre-split files
   - num_processors: 4       # 1 per CPU core (multiprocessing)
   - num_writers: 2          # DB writer threads
   - raw_queue_max: 20       # Reader → Processors
   - processed_queue_max: 10 # Processors → Writers
   - chunk_size: 65536       # 64KB read buffer
   - batch_size: 5000        # Records per DB batch
2. dev_config.yaml, test_config.yaml, prod_config.yaml
3. ConfigLoader class with vault support
4. DB2 connection settings based on the original job

Follow patterns from 03-configuration-management.md.
Generate files in python/[job_name]/config/
```

### Phase 6: Pipeline Orchestrator

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
Generate python/[job_name]/src/pipeline/ and src/batch/app.py
```

### Phase 7: Reconciliation (Multi-Worker)

```
@workspace For the [JOB_NAME] modernization, add reconciliation:

1. MetricsAggregator to collect counts from all reader/writer workers
2. Aggregate: SUM(worker_records_read) vs staging table count
3. Staging count vs final table count (after MERGE)
4. Optional: amount totals validation
5. Save results to RECON_AUDIT table with per-worker breakdown
6. Fail job if reconciliation fails

Follow patterns from 04-reconciliation.md.
Generate python/[job_name]/src/core/services/reconciliation_service.py
```

### Phase 8: Testing

```
@workspace For the [JOB_NAME] modernization, create tests:

1. Unit tests for domain models and validators
2. Unit tests for IO layer with mock EBCDIC data
3. Unit tests for repository layer with mocked connections
4. Integration test outline for full flow
5. Test fixtures with sample data

Generate files in python/[job_name]/tests/
```

---

## Specific Task Prompts

### Copybook to Domain Model

```
@workspace Convert the copybook source/jobs/[JOB_NAME]/copybooks/[COPYBOOK].cpy 
to a Python @dataclass. Handle all COMP-3 fields and include field validation.
```

### JCL Analysis Only

```
@workspace Analyze source/jobs/[JOB_NAME]/jcl/[JCL_FILE].jcl.
List all steps, programs executed, DD statements (files), and DB2 operations.
Explain the job flow in plain English.
```

### Single COBOL Program

```
@workspace Modernize the COBOL program source/jobs/[JOB_NAME]/programs/[PROGRAM].cbl.
The copybook is [COPYBOOK].cpy. Generate the Python equivalent following our patterns.
```

### Create Staging Table DDL

```
@workspace Based on the copybook source/jobs/[JOB_NAME]/copybooks/[COPYBOOK].cpy,
create the DB2 staging table DDL with RUN_ID column and proper indexes.
Follow our staging table pattern from copilot-instructions.md.
```

### Add Logging to Existing Code

```
@workspace Add BatchLogger logging to python/[job_name]/src/batch/app.py.
Include job start/end, batch progress, validation errors, and DB operations.
Follow patterns from 05-logging-monitoring.md.
```

---

## Troubleshooting Prompts

### Code Review

```
@workspace Review python/[job_name]/ for compliance with our modernization patterns.
Check: streaming file processing, batch DB inserts, connection pooling, 
reconciliation, logging, and error handling.
```

### Fix Issues

```
@workspace The [job_name] batch is failing with [ERROR].
Check the code against our patterns and suggest fixes.
```

### Performance Check

```
@workspace Review python/[job_name]/ for performance issues.
Verify: batch sizes, connection pooling, streaming patterns, and memory usage.
```

---

## Dependency Management

### Generate/Update requirements.txt

```
@workspace Scan python/[job_name]/src/ for all imports and generate:

1. requirements.txt with all production dependencies:
   - Pin versions with >= (e.g., ibm-db>=3.1.0)
   - Group by category (Core, DB, Config, Utilities)
   - Include comments for non-obvious packages

2. requirements-dev.txt with development/test dependencies:
   - pytest, pytest-cov, pytest-mock
   - Any testing utilities

Ensure every import in the code has a corresponding entry.
```

### Verify Dependencies

```
@workspace Check python/[job_name]/ for missing dependencies:
1. Scan all .py files for import statements
2. Compare against requirements.txt
3. Report any missing packages
4. Suggest version pins based on current stable releases
```

---

## Folder Structure Template

```
source/jobs/[JOB_NAME]/
├── jcl/
│   └── [JOB].jcl
├── programs/
│   ├── [STEP1].cbl
│   ├── [STEP2].cbl
│   └── ...
├── copybooks/
│   ├── [INPUT].cpy
│   ├── [OUTPUT].cpy
│   └── ...
├── sql/
│   └── [TABLES].sql
└── samples/              # Optional
    └── sample_data.txt

python/[job_name]/
├── src/
│   ├── core/
│   │   ├── models.py
│   │   ├── validators.py
│   │   └── services/
│   │       └── reconciliation_service.py
│   ├── io_layer/
│   │   ├── ebcdic_reader.py
│   │   ├── copybook_parser.py
│   │   ├── packed_decimal.py
│   │   └── file_splitter.py      # For multi-reader mode
│   ├── pipeline/                  # Hybrid producer-consumer
│   │   ├── orchestrator.py        # Mode selection, lifecycle
│   │   ├── single_reader.py       # SingleReader + Processor
│   │   ├── chunk_file_reader.py   # For multi-reader mode
│   │   └── db_writer.py           # threading.Thread based
│   ├── repository/
│   │   ├── connection_factory.py  # One conn per writer thread
│   │   ├── staging_repository.py
│   │   ├── final_repository.py
│   │   └── retry_policy.py
│   ├── batch/
│   │   ├── app.py
│   │   └── shutdown_handler.py
│   └── util/
│       ├── config_loader.py
│       ├── worker_logger.py       # Per-worker logging
│       ├── metrics_aggregator.py
│       └── pii_masker.py
├── config/
│   ├── base_config.yaml
│   └── [env]_config.yaml
├── requirements.txt          # All dependencies with pinned versions
├── requirements-dev.txt      # Dev/test dependencies (pytest, etc.)
└── tests/
    ├── unit/
    ├── integration/
    └── fixtures/
```

---

## Tips for Best Results

1. **Start with JCL** - It shows the complete job flow
2. **Include copybooks** - They define your data models
3. **One job at a time** - Complete one before starting another
4. **Phase approach for complex jobs** - More control, better results
5. **Review generated code** - Validate against your requirements
6. **Test incrementally** - Run tests after each phase

---

## Example: First Modernization

```
@workspace I'm starting my first modernization project.

The job is in source/jobs/DAILY_CUSTOMER_BATCH/.
It has:
- 1 JCL file (CUSTDALY.jcl)
- 3 COBOL programs (CUSTEXTR.cbl, CUSTVALD.cbl, CUSTLOAD.cbl)
- 2 copybooks (CUSTREC.cpy, CUSTERR.cpy)
- 1 DB2 table (CUSTOMER)

Please:
1. Analyze the complete job
2. Create a modernization plan
3. Then generate the full Python project following our architecture

Output to python/daily_customer_batch/
```

---

*Use these prompts with GitHub Copilot in VS Code. The copilot-instructions.md file provides the architectural patterns automatically.*

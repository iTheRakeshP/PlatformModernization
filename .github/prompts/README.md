# How To Use Python-Batch-Expert Prompts

> **Quick Start Guide** for modernizing COBOL batch jobs to Python using the prompt files in this directory.

---

## Overview

This guide walks you through the **7 main phases** of batch modernization, plus utility prompts for common tasks.

### Phase Summary

| Phase | Prompt File | Instruction File | Purpose |
|-------|------------|------------------|---------|
| **1** | `01-domain-models.prompt.md` | `01-ebcdic-copybook-handling.instructions.md` | Project structure & dataclasses from copybooks |
| **2** | `02-io-layer.prompt.md` | `01-ebcdic-copybook-handling.instructions.md` | EBCDIC file reading, COMP-3 handling |
| **3** | `03-repository.prompt.md` | `02-repository-pattern.instructions.md` | DB2 access, DbWriter threads, staging tables |
| **4** | `04-configuration.prompt.md` | `03-configuration-management.instructions.md` | YAML configs, environment settings |
| **5** | `05-pipeline.prompt.md` | `00-core-architecture.instructions.md` | Hybrid producer-consumer pipeline |
| **6** | `06-reconciliation.prompt.md` | `04-reconciliation.instructions.md` | Count validation, audit trails |
| **7** | `07-testing.prompt.md` | `06-testing-patterns.instructions.md` | Unit & integration tests |

### Utility Prompts (Use Anytime)

| Prompt File | Purpose |
|------------|---------|
| `08-specific-tasks.prompt.md` | Ad-hoc tasks (single copybook, add logging, etc.) |
| `09-troubleshooting.prompt.md` | Debug issues, performance problems |
| `10-dependency-management.prompt.md` | Generate/verify requirements.txt |
| `11-security-compliance.prompt.md` | PII masking, encryption, audit (uses `07-security-compliance.instructions.md`) |
| `12-performance-tuning.prompt.md` | Memory profiling, throughput tuning (uses `08-performance-tuning.instructions.md`) |

---

## Prerequisites

Before starting modernization, ensure you have:

1. **COBOL/JCL source files** in `source/jobs/[JOB_NAME]/`
   ```
   source/jobs/[JOB_NAME]/
   ├── jcl/           # JCL files
   ├── programs/      # COBOL programs
   ├── copybooks/     # COBOL copybooks
   └── sql/           # DB2 DDL (if any)
   ```

2. **Documentation generated** by `@COBOL-Reverse-Engineering-Expert` in `docs/[JOB_NAME]/`
   - If not available, run the reverse engineering agent first

---

## Step-by-Step Workflow

### Step 0: Verify Documentation Exists

Before starting, check if documentation exists:

```
@workspace Check if docs/[JOB_NAME]/ exists and has modernization-spec.md
```

If documentation doesn't exist:
```
@COBOL-Reverse-Engineering-Expert Analyze the job in source/jobs/[JOB_NAME]/
and generate complete documentation in docs/[JOB_NAME]/
```

---

### Phase 1: Project Structure & Domain Models

**Goal**: Create Python project structure and generate dataclasses from copybooks.

**Prompt**:
```
@Python-Batch-Expert Based on the analysis of [JOB_NAME], create:

1. Python project structure in python/[job_name]/ per 00-core-architecture.instructions.md
2. Domain models (@dataclass) from all copybooks in source/jobs/[JOB_NAME]/copybooks/
3. Handle all COMP-3 packed decimal fields properly
4. Include validation rules based on field definitions

Generate the src/entity/models.py and src/processor/validators.py files.
```

**Output**: 
```
python/[job_name]/
├── config/
├── src/
│   ├── entity/models.py        ← Generated
│   ├── processor/validators.py ← Generated
│   └── ...
└── tests/
```

---

### Phase 2: IO Layer

**Goal**: Implement EBCDIC file reading with chunked I/O.

**Prompt**:
```
@Python-Batch-Expert For the [JOB_NAME] modernization, implement the IO layer:

1. FilePartitioner for splitting 100GB+ files by byte ranges
2. PartitionedEBCDICReader with 64KB chunked reading
3. CopybookParser with field definitions from copybooks
4. Handle all data types: PIC X, PIC 9, COMP-3, COMP

Follow patterns from 01-ebcdic-copybook-handling.instructions.md.
Generate files in python/[job_name]/src/io_layer/
```

**Output**:
```
python/[job_name]/src/io_layer/
├── ebcdic_reader.py
├── copybook_parser.py
├── packed_decimal.py
└── file_partitioner.py
```

---

### Phase 3: Repository Layer

**Goal**: Implement DB2 access with threading pattern.

**Prompt**:
```
@Python-Batch-Expert For the [JOB_NAME] modernization, implement the repository layer:

1. ConnectionFactory (NOT connection pool - each DbWriter thread gets own connection)
2. DbWriter class extending threading.Thread
3. StagingRepository with RUN_ID + WORKER_ID columns
4. FinalRepository with MERGE operations
5. RetryPolicy decorator with exponential backoff

Follow patterns from 02-repository-pattern.instructions.md.
Generate files in python/[job_name]/src/repository/
```

**Output**:
```
python/[job_name]/src/repository/
├── connection_factory.py
├── db_writer.py
├── staging_repository.py
├── final_repository.py
└── retry_policy.py
```

---

### Phase 4: Configuration

**Goal**: Create YAML configuration files.

**Prompt**:
```
@Python-Batch-Expert For the [JOB_NAME] modernization, create configuration:

1. base_config.yaml with hybrid pipeline settings
2. dev_config.yaml, test_config.yaml, prod_config.yaml
3. ConfigLoader class with vault support

Follow patterns from 03-configuration-management.instructions.md.
Generate files in python/[job_name]/config/
```

**Output**:
```
python/[job_name]/
├── config/
│   ├── base_config.yaml
│   ├── dev_config.yaml
│   ├── test_config.yaml
│   └── prod_config.yaml
└── src/utility/
    └── config_loader.py
```

---

### Phase 5: Pipeline Orchestrator

**Goal**: Implement the hybrid producer-consumer pipeline.

**Prompt**:
```
@Python-Batch-Expert For the [JOB_NAME] modernization, implement the hybrid pipeline:

1. PipelineOrchestrator class with hybrid parallelism:
   - Processors: multiprocessing.Process (CPU-bound)
   - Writers: threading.Thread (I/O-bound)
2. Queue bridge pattern (mp.Queue → queue.Queue)
3. SingleReader in main thread
4. Bounded queues for backpressure control
5. Graceful shutdown with poison pills
6. RUN_ID + WORKER_ID traceability

Memory budget: < 500MB total.
Generate python/[job_name]/src/processor/ and src/batch/app.py
```

**Output**:
```
python/[job_name]/src/
├── processor/
│   ├── orchestrator.py
│   ├── single_reader.py
│   ├── processor.py
│   ├── queue_adapter.py
│   └── db_writer.py
└── batch/
    ├── app.py
    └── shutdown_handler.py
```

---

### Phase 6: Reconciliation

**Goal**: Implement count validation and audit trails.

**Prompt**:
```
@Python-Batch-Expert For the [JOB_NAME] modernization, add reconciliation:

1. MetricsAggregator to collect counts from all workers
2. Aggregate: SUM(worker_records) vs staging table count
3. Staging count vs final table count (after MERGE)
4. Save results to RECON_AUDIT table
5. Fail job if reconciliation fails

Follow patterns from 04-reconciliation.instructions.md.
Generate python/[job_name]/src/processor/reconciliation_service.py
```

---

### Phase 7: Testing

**Goal**: Generate comprehensive test suite.

**Prompt**:
```
@Python-Batch-Expert For the [JOB_NAME] modernization, create tests:

1. Unit tests for domain models and validators
2. Unit tests for IO layer with mock EBCDIC data
3. Unit tests for repository layer with mocked connections
4. Integration test outline for full flow
5. Test fixtures with sample data

Generate files in python/[job_name]/tests/
```

**Output**:
```
python/[job_name]/tests/
├── conftest.py
├── unit/
│   ├── test_models.py
│   ├── test_validators.py
│   ├── test_ebcdic_reader.py
│   └── test_repository.py
├── integration/
│   └── test_pipeline.py
└── fixtures/
    └── sample_input.dat
```

---

## Utility Prompts

### Generate requirements.txt

```
@Python-Batch-Expert Scan python/[job_name]/src/ for all imports and generate:
1. requirements.txt with all production dependencies
2. requirements-dev.txt with test dependencies
```

### Add Security Controls

```
@Python-Batch-Expert Add security controls to python/[job_name]/:
1. PII masking in logs
2. Secrets management with Vault
3. Audit logging for compliance

Follow patterns from 07-security-compliance.instructions.md.
```

### Performance Tuning

```
@Python-Batch-Expert Review python/[job_name]/ for performance:
1. Verify memory stays under 500MB
2. Optimize num_processors and num_writers
3. Tune batch_size for bulk inserts

Follow patterns from 08-performance-tuning.instructions.md.
```

### Troubleshooting

```
@Python-Batch-Expert The [job_name] batch is failing with [ERROR].
Check the code against our patterns and suggest fixes.
```

---

## Quick Reference: Complete Modernization Command

For a full modernization in one prompt (advanced users):

```
@Python-Batch-Expert Modernize the batch job [JOB_NAME]:

1. Read docs/[JOB_NAME]/modernization-spec.md for requirements
2. Create complete Python project in python/[job_name]/
3. Generate all layers: entity, io_layer, processor, repository, utility, batch
4. Include configuration, logging, reconciliation, and tests
5. Generate requirements.txt

Follow all instruction files in .github/instructions/.
```

---

## File Mapping Reference

| What You Need | Prompt File | Instruction File |
|--------------|-------------|------------------|
| Dataclasses from copybooks | 01-domain-models | 01-ebcdic-copybook-handling |
| EBCDIC file reading | 02-io-layer | 01-ebcdic-copybook-handling |
| DB2 repository pattern | 03-repository | 02-repository-pattern |
| YAML configuration | 04-configuration | 03-configuration-management |
| Pipeline architecture | 05-pipeline | 00-core-architecture |
| Count reconciliation | 06-reconciliation | 04-reconciliation |
| Unit/integration tests | 07-testing | 06-testing-patterns |
| Logging setup | 08-specific-tasks | 05-logging-monitoring |
| Security/PII masking | 11-security-compliance | 07-security-compliance |
| Performance tuning | 12-performance-tuning | 08-performance-tuning |

---

## Tips

1. **Always start with Phase 1** - Project structure is required for all other phases
2. **Run phases in order** - Each phase builds on the previous
3. **Use `@Python-Batch-Expert`** - This agent knows all the patterns
4. **Check documentation first** - Run `@COBOL-Reverse-Engineering-Expert` if docs don't exist
5. **Generate requirements.txt** - Do this after completing all phases
6. **Test incrementally** - Run tests after each phase to catch issues early

---

*Last Updated: December 4, 2025*

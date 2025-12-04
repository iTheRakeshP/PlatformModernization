# Phase 5: Configuration

> **Use Case**: Create configuration files with pipeline tuning parameters, environment-specific settings, and secrets management.

---

## Prompt

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

---

## Expected Files

```
python/[job_name]/
├── config/
│   ├── base_config.yaml       # Shared settings
│   ├── dev_config.yaml        # Development overrides
│   ├── test_config.yaml       # Test environment
│   ├── prod_config.yaml       # Production settings
│   └── secrets/               # .gitignored
│       └── .gitkeep
└── src/util/
    └── config_loader.py       # Load & merge configs
```

---

## Key Configuration Structure

### base_config.yaml
```yaml
application:
  name: "[JOB_NAME]_batch"
  log_level: "INFO"

pipeline:
  reading_mode: "single"
  num_processors: 4
  num_writers: 2
  raw_queue_max: 20
  processed_queue_max: 10

file_processing:
  encoding: "cp037"
  chunk_size: 65536
  record_length: 500  # From copybook

batch:
  batch_size: 5000
  max_error_threshold: 100

db2:
  driver: "ibm_db_dbi"
  retry:
    max_attempts: 3
    backoff_factor: 2

reconciliation:
  enabled: true
  count_check: true
```

### Environment Override (prod_config.yaml)
```yaml
pipeline:
  num_processors: 8
  num_writers: 4

batch:
  batch_size: 10000
```

---

## Prerequisites
- Repository layer from `04-repository.prompt.md`

---

## Next Steps
Proceed to `06-pipeline.prompt.md` for orchestrator

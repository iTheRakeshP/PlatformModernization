# Phase 3: Repository Layer

> **Use Case**: Implement DB2 access with the DbWriter threading pattern, staging tables, and MERGE operations.

---

## Prompt

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

---

## Create Staging Table DDL

```
@workspace Based on the copybook source/jobs/[JOB_NAME]/copybooks/[COPYBOOK].cpy,
create the DB2 staging table DDL with RUN_ID column and proper indexes.
Follow our staging table pattern from copilot-instructions.md.
```

---

## Expected Files

```
python/[job_name]/src/repository/
├── __init__.py
├── connection_factory.py      # Create connections (not pool)
├── db_writer.py               # threading.Thread with own connection
├── staging_repository.py      # Bulk insert to staging
├── final_repository.py        # MERGE to final tables
├── reconciliation_repository.py  # Audit trail
└── retry_policy.py            # Exponential backoff decorator
```

---

## Key Patterns

### DbWriter (One Connection Per Thread)
```python
class DbWriter(threading.Thread):
    def run(self) -> None:
        # Each writer creates its OWN connection (not shared)
        conn = ibm_db_dbi.connect(self.db_config['connection_string'])
        repository = StagingRepository(conn)
        
        while True:
            batch = self.input_queue.get(timeout=30)
            if batch is None:  # Poison pill
                break
            repository.bulk_insert(batch.records, self.run_id, self.writer_id)
```

### Staging Table with RUN_ID + WORKER_ID
```sql
CREATE TABLE STG_BATCH (
    STG_ID          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    RUN_ID          VARCHAR(50) NOT NULL,
    WORKER_ID       INTEGER NOT NULL,
    -- data columns...
    INSERT_TS       TIMESTAMP DEFAULT CURRENT TIMESTAMP
);
```

---

## Prerequisites
- Domain models from `01-domain-models.prompt.md`
- IO layer from `02-io-layer.prompt.md`

---

## Next Steps
Proceed to `04-configuration.prompt.md` for config files

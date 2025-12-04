# Phase 6: Reconciliation

> **Use Case**: Implement multi-worker reconciliation to verify record counts across all pipeline stages.

---

## Prompt

```
@workspace For the [JOB_NAME] modernization, add reconciliation:

1. MetricsAggregator to collect counts from all reader/writer workers
2. Aggregate: SUM(worker_records_read) vs staging table count
3. Staging count vs final table count (after MERGE)
4. Optional: amount totals validation
5. Save results to RECON_AUDIT table with per-worker breakdown
6. Fail job if reconciliation fails

Follow patterns from 04-reconciliation.instructions.md.
Generate python/[job_name]/src/processor/reconciliation_service.py
```

---

## Expected Files

```
python/[job_name]/src/processor/
├── __init__.py
└── reconciliation_service.py
```

---

## Key Patterns

### MetricsAggregator
```python
@dataclass
class WorkerMetrics:
    worker_id: int
    worker_type: str  # 'processor' or 'writer'
    records_processed: int
    errors_count: int
    start_time: datetime
    end_time: datetime

class MetricsAggregator:
    def aggregate(self, worker_metrics: List[WorkerMetrics]) -> AggregatedMetrics:
        return AggregatedMetrics(
            total_read=sum(m.records_processed for m in worker_metrics if m.worker_type == 'processor'),
            total_written=sum(m.records_processed for m in worker_metrics if m.worker_type == 'writer'),
            total_errors=sum(m.errors_count for m in worker_metrics)
        )
```

### Reconciliation Checks
```python
class ReconciliationService:
    def reconcile_counts(self, run_id: str, aggregated: AggregatedMetrics) -> List[ReconciliationResult]:
        results = []
        
        # Check 1: Processed vs Staging
        staging_count = self._staging_repo.get_record_count(run_id)
        results.append(ReconciliationResult(
            check_name='processed_to_staging',
            expected_count=aggregated.total_read - aggregated.total_errors,
            actual_count=staging_count,
            passed=(expected == staging_count)
        ))
        
        # Check 2: Staging vs Final (after MERGE)
        final_count = self._final_repo.get_count_for_run(run_id)
        results.append(ReconciliationResult(
            check_name='staging_to_final',
            expected_count=staging_count,
            actual_count=final_count,
            passed=(staging_count == final_count)
        ))
        
        return results
```

### RECON_AUDIT Table
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

## Prerequisites
- Pipeline orchestrator from `05-pipeline.prompt.md`
- Repository layer from `03-repository.prompt.md`

---

## Next Steps
Proceed to `07-testing.prompt.md` for test generation

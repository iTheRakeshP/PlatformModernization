# Reconciliation Framework

## Concept

Multi-level data validation ensuring source data integrity through the entire pipeline (source → staging → final). Tracks record counts, hash validations, and business rule checks with audit trail. Critical for mainframe modernization where data accuracy is paramount.

**Hybrid Pipeline Context**: With processors (multiprocessing) and writer threads (threading), reconciliation must aggregate counts from all workers:

```
┌─────────────────────────────────────────────────────────────────┐
│                     RECONCILIATION FLOW                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Source File ──► SingleReader ──► Processors (4 Processes)     │
│      │                                    │                      │
│      │                                    ▼                      │
│  SOURCE_COUNT              PROCESSOR_1 + PROCESSOR_2 +          │
│  (pre-read)                PROCESSOR_3 + PROCESSOR_4 counts     │
│      │                                    │                      │
│      │                                    ▼                      │
│      │                           Writers (2 Threads)            │
│      │                                    │                      │
│      └──────────────► COMPARE ◄──── SUM(WRITER_COUNTS)          │
│                          │                                       │
│                          ▼                                       │
│                  STAGING_TABLE COUNT                             │
│                  (GROUP BY RUN_ID, WORKER_ID)                    │
│                          │                                       │
│                          ▼                                       │
│                  FINAL_TABLE COUNT                               │
│                  (WHERE LAST_RUN_ID = ?)                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## When to Use

✅ **Use when:**
- Moving data from mainframe to distributed systems
- Financial or regulatory data (SOX, audit requirements)
- Need proof of data integrity
- Restartability and reprocessing scenarios
- Multi-step data pipelines

❌ **Simpler alternatives when:**
- Non-critical data pipelines
- Real-time streaming (use different patterns)
- Internal-only data with no compliance needs

## Complete Implementation

### 1. Reconciliation Models

```python
from dataclasses import dataclass
from datetime import datetime
from typing import List, Optional
from enum import Enum

class ReconciliationType(Enum):
    COUNT = "count"
    HASH = "hash"
    BUSINESS_RULE = "business_rule"
    AMOUNT_TOTAL = "amount_total"

@dataclass
class ReconciliationResult:
    """Result of a single reconciliation check."""
    run_id: str
    check_name: str
    check_type: ReconciliationType
    expected_value: any
    actual_value: any
    passed: bool
    discrepancy: any = None
    error_message: Optional[str] = None
    check_timestamp: datetime = None
    
    def __post_init__(self):
        if self.check_timestamp is None:
            self.check_timestamp = datetime.now()
        
        if not self.passed and self.error_message is None:
            self.error_message = f"Expected {self.expected_value}, got {self.actual_value}"

@dataclass
class ReconciliationReport:
    """Complete reconciliation report for a run."""
    run_id: str
    job_name: str
    start_time: datetime
    end_time: datetime
    total_checks: int
    passed_checks: int
    failed_checks: int
    results: List[ReconciliationResult]
    
    @property
    def success_rate(self) -> float:
        """Calculate percentage of passed checks."""
        if self.total_checks == 0:
            return 0.0
        return (self.passed_checks / self.total_checks) * 100
    
    @property
    def all_passed(self) -> bool:
        """Check if all reconciliation checks passed."""
        return self.failed_checks == 0
```

### 2. Reconciliation Service

```python
from decimal import Decimal
import hashlib
import logging

logger = logging.getLogger(__name__)


class ReconciliationService:
    """
    Multi-level reconciliation service.
    Validates data integrity at each stage of the pipeline.
    """
    
    def __init__(self, 
                 staging_repo,
                 final_repo,
                 recon_repo):
        self._staging_repo = staging_repo
        self._final_repo = final_repo
        self._recon_repo = recon_repo
    
    def reconcile_pipeline(self, 
                          run_id: str,
                          source_count: int,
                          source_hash: Optional[str] = None,
                          source_amount_total: Optional[Decimal] = None) -> ReconciliationReport:
        """
        Perform complete pipeline reconciliation.
        
        Args:
            run_id: Batch run identifier
            source_count: Count of records in source file
            source_hash: Optional hash of source data
            source_amount_total: Optional sum of amounts from source
        
        Returns:
            ReconciliationReport with all check results
        """
        start_time = datetime.now()
        results = []
        
        # Check 1: Source → Staging count
        staging_count = self._staging_repo.get_record_count(run_id)
        results.append(ReconciliationResult(
            run_id=run_id,
            check_name='source_to_staging_count',
            check_type=ReconciliationType.COUNT,
            expected_value=source_count,
            actual_value=staging_count,
            passed=(source_count == staging_count),
            discrepancy=(staging_count - source_count)
        ))
        
        # Check 2: Staging → Final count
        final_count = self._final_repo.get_count_for_run(run_id)
        results.append(ReconciliationResult(
            run_id=run_id,
            check_name='staging_to_final_count',
            check_type=ReconciliationType.COUNT,
            expected_value=staging_count,
            actual_value=final_count,
            passed=(staging_count == final_count),
            discrepancy=(final_count - staging_count)
        ))
        
        # Check 3: Amount totals (if provided)
        if source_amount_total is not None:
            staging_total = self._staging_repo.get_amount_total(run_id)
            results.append(ReconciliationResult(
                run_id=run_id,
                check_name='source_to_staging_amount',
                check_type=ReconciliationType.AMOUNT_TOTAL,
                expected_value=float(source_amount_total),
                actual_value=float(staging_total),
                passed=(abs(source_amount_total - staging_total) < Decimal('0.01')),
                discrepancy=float(staging_total - source_amount_total)
            ))
        
        # Check 4: Hash validation (if provided)
        if source_hash:
            staging_hash = self._compute_staging_hash(run_id)
            results.append(ReconciliationResult(
                run_id=run_id,
                check_name='source_to_staging_hash',
                check_type=ReconciliationType.HASH,
                expected_value=source_hash,
                actual_value=staging_hash,
                passed=(source_hash == staging_hash),
                discrepancy=None
            ))
        
        # Create report
        end_time = datetime.now()
        passed_checks = sum(1 for r in results if r.passed)
        
        report = ReconciliationReport(
            run_id=run_id,
            job_name='batch_pipeline',
            start_time=start_time,
            end_time=end_time,
            total_checks=len(results),
            passed_checks=passed_checks,
            failed_checks=len(results) - passed_checks,
            results=results
        )
        
        # Save results to audit table
        self._recon_repo.save_reconciliation_report(report)
        
        # Log summary
        if report.all_passed:
            logger.info(f"Reconciliation PASSED for RUN_ID={run_id}: {report.passed_checks}/{report.total_checks} checks")
        else:
            logger.error(f"Reconciliation FAILED for RUN_ID={run_id}: {report.passed_checks}/{report.total_checks} checks")
            for result in report.results:
                if not result.passed:
                    logger.error(f"  Failed check: {result.check_name} - {result.error_message}")
        
        return report
    
    def _compute_staging_hash(self, run_id: str) -> str:
        """
        Compute hash of staging data for verification.
        Useful for detecting data corruption or tampering.
        """
        records = self._staging_repo.get_records_for_hash(run_id)
        
        hasher = hashlib.sha256()
        for record in records:
            # Hash key fields in deterministic order
            record_string = f"{record.acct_id}|{record.prod_code}|{record.tran_amt}|{record.tran_date}"
            hasher.update(record_string.encode('utf-8'))
        
        return hasher.hexdigest()
    
    def compute_source_hash(self, records: List) -> str:
        """Compute hash of source records for later comparison."""
        hasher = hashlib.sha256()
        for record in records:
            record_string = f"{record.acct_id}|{record.prod_code}|{record.tran_amt}|{record.tran_date}"
            hasher.update(record_string.encode('utf-8'))
        return hasher.hexdigest()
```

### 3. Multi-Worker Metrics Aggregation

When using the producer-consumer pipeline with multiple workers, aggregate counts from all workers before reconciliation:

```python
from dataclasses import dataclass
from typing import Dict, List
from multiprocessing import Manager

@dataclass
class WorkerMetrics:
    """Metrics from a single worker."""
    worker_id: int
    records_read: int = 0
    records_processed: int = 0
    records_written: int = 0
    records_errored: int = 0
    batches_written: int = 0


class MetricsAggregator:
    """
    Aggregates metrics from all pipeline workers.
    Uses multiprocessing.Manager for cross-process access.
    """
    
    def __init__(self, manager: Manager = None):
        """Initialize with optional multiprocessing Manager."""
        self._manager = manager or Manager()
        self._worker_metrics: Dict[int, dict] = self._manager.dict()
    
    def report_reader_metrics(self, worker_id: int, records_read: int, 
                               records_processed: int, records_errored: int) -> None:
        """Report metrics from a reader worker."""
        self._worker_metrics[f'reader_{worker_id}'] = {
            'type': 'reader',
            'worker_id': worker_id,
            'records_read': records_read,
            'records_processed': records_processed,
            'records_errored': records_errored
        }
    
    def report_writer_metrics(self, writer_id: int, records_written: int,
                               batches_written: int) -> None:
        """Report metrics from a DB writer worker."""
        self._worker_metrics[f'writer_{writer_id}'] = {
            'type': 'writer',
            'writer_id': writer_id,
            'records_written': records_written,
            'batches_written': batches_written
        }
    
    def get_totals(self) -> Dict[str, int]:
        """Aggregate all worker metrics into totals."""
        totals = {
            'total_records_read': 0,
            'total_records_processed': 0,
            'total_records_written': 0,
            'total_records_errored': 0,
            'total_batches_written': 0,
            'num_readers': 0,
            'num_writers': 0
        }
        
        for key, metrics in self._worker_metrics.items():
            if metrics['type'] == 'reader':
                totals['total_records_read'] += metrics['records_read']
                totals['total_records_processed'] += metrics['records_processed']
                totals['total_records_errored'] += metrics['records_errored']
                totals['num_readers'] += 1
            elif metrics['type'] == 'writer':
                totals['total_records_written'] += metrics['records_written']
                totals['total_batches_written'] += metrics['batches_written']
                totals['num_writers'] += 1
        
        return totals
    
    def get_per_worker_breakdown(self) -> List[Dict]:
        """Get detailed breakdown per worker for debugging."""
        return list(self._worker_metrics.values())


# Usage in PipelineOrchestrator:
class PipelineOrchestrator:
    def run(self, input_file: str) -> Dict:
        manager = Manager()
        metrics_aggregator = MetricsAggregator(manager)
        
        # ... start workers, passing metrics_aggregator ...
        
        # After all workers complete:
        totals = metrics_aggregator.get_totals()
        
        # Reconcile using aggregated totals
        recon_service = ReconciliationService(...)
        report = recon_service.reconcile_pipeline(
            run_id=self.run_id,
            source_count=totals['total_records_read']  # From all readers
        )
        
        return {
            'worker_metrics': totals,
            'reconciliation': report
        }
```

### 4. Staging Table with WORKER_ID

For per-worker traceability, include `WORKER_ID` in staging table:

```sql
-- Query to reconcile per-worker counts
SELECT WORKER_ID, COUNT(*) AS RECORD_COUNT
FROM STG_BATCH
WHERE RUN_ID = ?
GROUP BY WORKER_ID
ORDER BY WORKER_ID;

-- Total count for reconciliation
SELECT COUNT(*) AS TOTAL_RECORDS
FROM STG_BATCH
WHERE RUN_ID = ?;
```



# Troubleshooting Prompts

> **Use Case**: Debug issues, review code compliance, and fix problems in modernized batch jobs.

---

## Code Review

```
@workspace Review python/[job_name]/ for compliance with our modernization patterns.
Check: streaming file processing, batch DB inserts, connection pooling, 
reconciliation, logging, and error handling.
```

---

## Fix Issues

```
@workspace The [job_name] batch is failing with [ERROR].
Check the code against our patterns and suggest fixes.
```

---

## Performance Check

```
@workspace Review python/[job_name]/ for performance issues.
Verify: batch sizes, connection pooling, streaming patterns, and memory usage.
```

---

## Memory Analysis

```
@workspace The [job_name] batch is using too much memory.
Review the code for:
1. Any full file loads (should be streaming)
2. Unbounded collections
3. Memory leaks in loops
4. Large batch accumulation
Suggest fixes to stay under 500MB memory budget.
```

---

## DB Connection Issues

```
@workspace The [job_name] batch has DB connection issues: [ERROR_DETAILS].
Review:
1. Connection factory implementation
2. DbWriter thread connection handling
3. Retry policy configuration
4. Connection cleanup on errors
```

---

## Reconciliation Failures

```
@workspace The [job_name] batch reconciliation is failing.
Expected: [X] records, Actual: [Y] records.
Help diagnose:
1. Where records might be lost
2. Worker metrics aggregation
3. Staging table counts
4. Any error handling that might skip records
```

---

## Pipeline Deadlock

```
@workspace The [job_name] batch appears to hang/deadlock.
Review:
1. Queue sizes and backpressure
2. Poison pill propagation
3. Thread/process join ordering
4. Exception handling in workers
```

---

## EBCDIC Parsing Issues

```
@workspace The [job_name] batch has data parsing errors: [DETAILS].
Review:
1. EBCDIC encoding (cp037 vs cp500)
2. Record length calculation
3. COMP-3 field handling
4. Field offset calculations from copybook
```

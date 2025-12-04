# Full Job Modernization

> **Use Case**: Complete end-to-end modernization of a COBOL batch job to Python in a single prompt.

---

## Detailed Prompt

```
@workspace Modernize the entire [JOB_NAME] batch job in source/jobs/[JOB_NAME]/.

Please:
1. Check docs/[JOB_NAME]/ for existing analysis from @COBOL-Reverse-Engineering-Expert
2. If docs exist, use modernization-spec.md as primary input
3. If no docs exist, analyze the JCL to understand the complete job flow
4. Review all COBOL programs and copybooks
5. Create a modernization plan showing COBOL → Python mapping
6. Generate the complete Python project using our hybrid producer-consumer pipeline:
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

---

## Simple Version

```
@workspace Modernize source/jobs/[JOB_NAME]/ to Python.
Use producer-consumer pipeline for 100GB+ files.
Follow our copilot-instructions architecture. Generate complete project in python/[job_name]/.
```

---

## First Modernization Example

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

## Tips
- Start with `@COBOL-Reverse-Engineering-Expert` for complex jobs
- Use phase-by-phase prompts when you need more control
- Review generated code against your requirements

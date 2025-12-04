# Specific Task Prompts

> **Use Case**: Individual prompts for specific modernization tasks outside the main phases.

---

## Copybook to Domain Model

```
@workspace Convert the copybook source/jobs/[JOB_NAME]/copybooks/[COPYBOOK].cpy 
to a Python @dataclass. Handle all COMP-3 fields and include field validation.
```

---

## JCL Analysis Only

```
@workspace Analyze source/jobs/[JOB_NAME]/jcl/[JCL_FILE].jcl.
List all steps, programs executed, DD statements (files), and DB2 operations.
Explain the job flow in plain English.
```

---

## Single COBOL Program

```
@workspace Modernize the COBOL program source/jobs/[JOB_NAME]/programs/[PROGRAM].cbl.
The copybook is [COPYBOOK].cpy. Generate the Python equivalent following our patterns.
```

---

## Create Staging Table DDL

```
@workspace Based on the copybook source/jobs/[JOB_NAME]/copybooks/[COPYBOOK].cpy,
create the DB2 staging table DDL with RUN_ID column and proper indexes.
Follow our staging table pattern from copilot-instructions.md.
```

---

## Add Logging to Existing Code

```
@workspace Add BatchLogger logging to python/[job_name]/src/batch/app.py.
Include job start/end, batch progress, validation errors, and DB operations.
Follow patterns from 05-logging-monitoring.md.
```

---

## Generate README

```
@workspace Create a README.md for python/[job_name]/ that includes:
1. Job purpose and description
2. Prerequisites and dependencies
3. Configuration instructions
4. How to run the batch
5. Monitoring and logging
6. Troubleshooting guide
```

---

## Add Error Handling

```
@workspace Review python/[job_name]/src/ and add comprehensive error handling:
1. Try/except blocks with specific exceptions
2. Graceful degradation where appropriate
3. Error logging with context
4. Proper cleanup on failure
Follow patterns from the instruction files.
```

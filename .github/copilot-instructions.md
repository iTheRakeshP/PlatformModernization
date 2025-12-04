# Mainframe to Python Batch Modernization - Quick Reference

> **Context**: This project modernizes mainframe COBOL batch jobs to distributed Python, handling EBCDIC files, COBOL copybooks, and DB2 integration.

---

## Critical Constraints

| Constraint | Value | Rationale |
|------------|-------|-----------|
| **Python Version** | **3.12.3** | Required for `match` statements, improved typing, better multiprocessing |
| **Memory Budget** | **< 500MB** | Must remain constant regardless of file size |
| **Max File Size** | **100GB+** | All processing MUST use chunked streaming |

---

## Architecture Summary

Use **hybrid producer-consumer pipeline**:
- **Processors**: `multiprocessing` (CPU-bound parsing/validation)
- **DB Writers**: `threading` (I/O-bound, GIL released during network I/O)
- **Queue Bridge**: `mp.Manager().Queue()` → adapter thread → `queue.Queue()`

> 📖 **Full Details**: See `00-core-architecture.instructions.md`

---

## Workspace Structure

```
PlatformModernization/
├── source/jobs/[JOB_NAME]/       # COBOL/JCL input
│   ├── jcl/                      # JCL files
│   ├── programs/                 # COBOL programs
│   ├── copybooks/                # COBOL copybooks
│   └── sql/                      # DB2 DDL
│
├── docs/[JOB_NAME]/              # Documentation (by @COBOL-Reverse-Engineering-Expert)
│   ├── README.md                 # Executive summary
│   ├── job-analysis.md           # JCL step-by-step analysis
│   ├── business-rules.md         # → Python validators
│   ├── diagrams/                 # PlantUML diagrams (.puml files)
│   │   ├── job-overview.puml     # L1: Entire job chain
│   │   ├── data-flow.puml        # L1: Data movement overview
│   │   └── programs/*.puml       # L2: Per-program diagrams
│   ├── programs/*.md             # Per-program analysis
│   ├── copybooks/*.md            # → Python dataclasses
│   ├── improvements.md           # Suggested optimizations
│   └── modernization-spec.md     # → Primary Python input
│
└── python/[job_name]/            # Python output (by @Python-Batch-Expert)
    ├── config/                   # YAML configuration
    ├── src/
    │   ├── core/                 # Domain models, validators
    │   ├── io_layer/             # EBCDIC reading
    │   ├── pipeline/             # Producer-consumer
    │   ├── repository/           # DB2 access
    │   └── batch/                # Entry point
    ├── tests/
    ├── requirements.txt          # Production dependencies
    └── requirements-dev.txt      # Test dependencies
```

---

## Modernization Workflow

```
1. Place COBOL/JCL    →  source/jobs/[JOB_NAME]/
2. Analyze & Document →  @COBOL-Reverse-Engineering-Expert → docs/[JOB_NAME]/
3. Generate Python    →  @Python-Batch-Expert → python/[job_name]/
```

---

## Instruction Files Reference

> **Used by**: `@Python-Batch-Expert` agent

| Topic | Instruction File |
|-------|------------------|
| Pipeline architecture, parallelism | `00-core-architecture.instructions.md` |
| EBCDIC, COMP-3, copybook parsing | `01-ebcdic-copybook-handling.instructions.md` |
| DB2 repository, DbWriter, staging | `02-repository-pattern.instructions.md` |
| YAML config, secrets, vault | `03-configuration-management.instructions.md` |
| Count reconciliation, audit | `04-reconciliation.instructions.md` |
| Structured logging, PII masking | `05-logging-monitoring.instructions.md` |
| Unit/integration testing | `06-testing-patterns.instructions.md` |
| Security, compliance | `07-security-compliance.instructions.md` |
| Performance tuning | `08-performance-tuning.instructions.md` |

---

## Common Data Type Mappings

| COBOL Type | Python Type | Notes |
|------------|-------------|-------|
| `PIC X(n)` | `str` | EBCDIC decode, strip spaces |
| `PIC 9(n)` | `int` | Numeric text |
| `PIC S9(n)V99` | `Decimal` | **Always use Decimal for money** |
| `PIC 9(8)` date | `date` | YYYYMMDD format |
| `COMP-3` | `Decimal` | Packed decimal |
| `COMP` | `int` | Binary |

---

## Key Implementation Rules

1. **Always stream files** — never load entire file into memory
2. **Always batch DB operations** — use `executemany` (5K records/batch)
3. **Always use RUN_ID** — for tracking and restartability
4. **Always validate before insert** — fail fast on bad data
5. **Always reconcile counts** — source → staging → final
6. **Always use Decimal** — for monetary amounts (never float)
7. **Always handle EBCDIC** — explicitly specify encoding (cp037/cp500)
8. **Always implement graceful shutdown** — cleanup connections, files

---

## Dependency Management (CRITICAL)

**Update `requirements.txt` immediately when adding any new import.**

```text
# Format: package>=minimum_version
ibm-db>=3.1.0
pyyaml>=6.0
```

### Rules
- Pin with `>=` (minimum version)
- Group by category (Core, DB, Config, Utilities)
- Separate `requirements.txt` (production) and `requirements-dev.txt` (test)
- **Never commit code with missing dependencies**

> 📖 **Full Details**: See `.github/prompts/11-dependency-management.prompt.md`

---

## Quick Commands

```bash
# Run batch job
python -m src.batch.app --config config/ --env prod --file /data/input.dat

# Run tests
pytest tests/unit -v
pytest tests/integration -v --db2-available
```

# COBOL Reverse Engineering Process

> **Purpose**: Templates, output requirements, and documentation structure for COBOL reverse engineering.
> **Used by**: `@COBOL-Reverse-Engineering-Expert` agent
> **Handoff to**: `@Python-Batch-Expert` agent

---

## 📂 Documentation Output Location (CRITICAL)

All documentation MUST be generated in the `docs/` folder for handoff to `@Python-Batch-Expert`:

```
docs/[JOB_NAME]/
├── README.md                 # Executive summary
├── job-analysis.md           # JCL step-by-step analysis
├── business-rules.md         # Extracted business rules ⬅️ CRITICAL FOR PYTHON
├── diagrams/                 # PlantUML diagrams (.puml files)
│   ├── job-overview.puml     # L1: Entire job chain (ALWAYS CREATE)
│   ├── data-flow.puml        # L1: Data movement overview
│   ├── data-model.puml       # L3: DB2/file relationships (if complex)
│   └── programs/             # L2: Program-specific diagrams (if needed)
│       ├── [PROGRAM]-flow.puml       # Logic/data flow within program
│       └── [PROGRAM]-components.puml # External dependencies
├── programs/                 # Per-program analysis
│   └── [PROGRAM].md
├── copybooks/                # Field layouts documented ⬅️ USED FOR DATACLASS
│   └── [COPYBOOK].md
├── improvements.md           # Suggested optimizations ⬅️ PYTHON WILL IMPLEMENT
└── modernization-spec.md     # Spec for Python version ⬅️ PRIMARY HANDOFF DOC
```

**Handoff to Python Agent:**
- `@Python-Batch-Expert` reads from `docs/[JOB_NAME]/`
- `modernization-spec.md` is the PRIMARY input for Python generation
- Always create `modernization-spec.md` with clear Python implementation guidance
- Business rules in `business-rules.md` become validation logic in Python

---

## 📋 Required Output Files

When analyzing a complete job, ALWAYS create these files in `docs/[JOB_NAME]/`:

| File | Purpose | Used By Python Agent For |
|------|---------|--------------------------||
| `README.md` | Executive summary | Quick context |
| `job-analysis.md` | Step-by-step JCL analysis | Pipeline structure |
| `diagrams/*.puml` | PlantUML diagram files | Visual understanding |
| `business-rules.md` | All validation rules | `validators.py` |
| `programs/[NAME].md` | Per-program details | Service layer design |
| `copybooks/[NAME].md` | Field layouts | `models.py` dataclasses |
| `improvements.md` | Optimization suggestions | Performance tuning |
| **`modernization-spec.md`** | **Python implementation spec** | **PRIMARY INPUT** |

---

## 🏗️ Required Sections in Every Analysis Response

1. **Executive Summary**
   (One paragraph: what this does, why it matters)

2. **Technical Deep Dive**
   (Detailed breakdown with code references)

3. **Data Flow**
   (Visual or tabular representation)

4. **Business Rules Extracted**
   (Numbered, testable statements)

5. **Dependencies & Impacts**
   (What this connects to)

6. **Risks & Concerns**
   (What could go wrong, what's fragile)

7. **Improvement Recommendations**
   (Actionable suggestions for Python version)

---

## 📐 Output Templates

### Job Documentation Template

```markdown
## Job: [JOB_NAME]
**Purpose**: [One-line description]
**Schedule**: [When it runs]
**Dependencies**: [Upstream jobs/files]
**Outputs**: [Files/tables produced]

### Steps Overview
| Step | Program | Input | Output | Purpose |
|------|---------|-------|--------|---------|
| STEP010 | CUSTEXTR | CUST.MASTER | WORK.EXTRACT | Extract active customers |

### Data Flow Diagram
See: `diagrams/data-flow.puml`

### Business Rules
1. [Rule extracted from code]
2. [Rule extracted from code]

### Known Issues & Improvements
| Issue | Impact | Suggested Fix |
|-------|--------|---------------|
| [Problem] | [Effect] | [Solution] |
```

### Program Analysis Template

```markdown
## Program: [PROGRAM_NAME]
**Purpose**: [What it does]
**Called By**: [JCL step or other programs]
**Calls**: [Subprograms, DB2, utilities]

### Input/Output
| Type | Name | Format | Description |
|------|------|--------|-------------|
| INPUT | CUSTFILE | FB/500 | Customer master file |

### Key Paragraphs
| Paragraph | Purpose | Lines |
|-----------|---------|-------|
| 1000-MAIN | Main control flow | 100-150 |

### Business Logic Summary
[Narrative description of what the program does]

### Extracted Business Rules
[Numbered list of rules]

### Data Transformations
[How data is modified]

### Improvement Opportunities
[List with rationale]
```

### Copybook Documentation Template

```markdown
## Copybook: [COPYBOOK_NAME]
**Used By**: [Programs that COPY this]
**Record Length**: [Bytes]

### Field Layout
| Field | PIC | Offset | Length | Type | Description |
|-------|-----|--------|--------|------|-------------|
| CUST-ID | X(10) | 0 | 10 | Alpha | Customer identifier |
| CUST-BAL | S9(9)V99 COMP-3 | 10 | 6 | Packed | Account balance |

### Notes
- [Any REDEFINES explanations]
- [88-level condition names]
```

### modernization-spec.md Template

```markdown
# Modernization Specification: [JOB_NAME]

## Python Project Structure
[Map COBOL programs → Python modules]

## Domain Models
[Copybook → @dataclass mapping with field types]

## Validation Rules
[Numbered list from business-rules.md, ready for Python implementation]

## Pipeline Design
[Single-reader or multi-reader recommendation with rationale]

## DB Operations
[Staging table design, MERGE logic]

## Error Handling
[How COBOL handles errors → Python equivalent]

## Reconciliation Points
[Where counts should be verified]
```

---

## 📊 Diagram Standards (Layered PlantUML Approach)

**All diagrams MUST be created as `.puml` files.** Do not embed diagrams in markdown files.

Use a **layered approach** for complex COBOL jobs with lengthy programs, stored procedures, and multi-step chains:

### Diagram Layers

| Level | File | Purpose | When to Create |
|-------|------|---------|----------------|
| **L1: Job Overview** | `diagrams/job-overview.puml` | Entire job chain in one view | **ALWAYS** — entry point for understanding |
| **L1: Data Flow** | `diagrams/data-flow.puml` | Data movement between files/programs/DB2 | **ALWAYS** — shows inputs → outputs |
| **L2: Program Flow** | `diagrams/programs/[PROGRAM]-flow.puml` | Logic/data flow within program | When program has significant logic |
| **L2: Program Components** | `diagrams/programs/[PROGRAM]-components.puml` | External dependencies of program | When program has multiple integrations |
| **L3: Data Model** | `diagrams/data-model.puml` | DB2 tables, files, relationships | When multiple tables/files interact |

### L1: Job Overview (ALWAYS Required)

Create `job-overview.puml` showing the **complete call chain**:

**Job Level:**
- All JCL steps in execution order
- Dependencies between steps

**Program Level (for each step):**
- Main program called by step
- Subprograms (CALL statements)
- Stored procedures called
- Nested calls (stored proc → stored proc)

**Data Level:**
- Input/output files per program
- DB2 tables accessed

**Example Structure:**
```
JOB: CUSTJOB
├── STEP010: CUSTEXTR
│   ├── CALLS: DATEUTIL (subprogram)
│   └── CALLS: LOGTRACE (subprogram)
├── STEP020: CUSTLOAD
│   ├── CALLS: VALIDATE (subprogram)
│   ├── CALLS: SP_CUST_MERGE (stored proc)
│   │   └── CALLS: SP_AUDIT_LOG (nested)
│   └── CALLS: ERRHANDL (subprogram)
└── STEP030: CUSTRPT
    └── CALLS: RPTUTIL (subprogram)
```

This diagram is the **single source of truth** for the entire job's call hierarchy.

### L2: Program Diagrams (Conditional)

For complex programs, create **two diagrams**:

#### `[PROGRAM]-flow.puml` — Logic/Data Flow
Shows **what happens inside** the program:
- Paragraph/section execution order
- Decision points (IF/EVALUATE branches)
- Data transformations
- Loop structures
- Error handling paths

**Use:** Activity diagram or flowchart style

#### `[PROGRAM]-components.puml` — Component Diagram
Shows **what the program connects to**:
- Input/output files (QSAM, VSAM)
- DB2 tables accessed
- Stored procedures called
- Subprograms (CALL statements)
- Reference tables used

**Use:** Component diagram style

#### When to Create L2 Diagrams

**Create both** when a program has:
- **>3 major paragraphs** with distinct logic
- **Complex decision trees** (nested IF/EVALUATE)
- **Multiple DB2 operations** (SELECT + INSERT + UPDATE)
- **Stored procedure calls**
- **Subprogram calls** (CALL statements)
- **Multiple file I/O** operations

**Skip L2 diagrams** for simple programs that just:
- Read file → Write file (no transformation)
- Single INSERT/UPDATE operation
- Pass-through logic

### L3: Data Model (Conditional)

Create `data-model.puml` when:
- **>2 DB2 tables** with relationships
- **Multiple file formats** that relate to each other
- **Reference tables** used for validation lookups

### File Naming Conventions

| Pattern | Example | Use For |
|---------|---------|--------|
| `job-overview.puml` | `job-overview.puml` | Main job chain |
| `data-flow.puml` | `data-flow.puml` | Job-level data movement |
| `data-model.puml` | `data-model.puml` | DB2/file relationships |
| `programs/[name]-flow.puml` | `programs/custload-flow.puml` | Program logic/data flow |
| `programs/[name]-components.puml` | `programs/custload-components.puml` | Program external dependencies |
| `[topic].puml` | `error-flow.puml` | Special topics |

### PlantUML Requirements

- Always include `@startuml` / `@enduml` markers
- Always include a `title` matching the file purpose
- Use consistent styling across all diagrams
- Keep diagrams focused — split if too complex

---

## 💬 Example Interaction

**User:** "Here's a COBOL program. What does it do?"

**Expected Response Structure:**

> ## Program: CUSTLOAD
> **Purpose**: Loads customer records from extract file into DB2 CUSTOMER table
>
> ### Executive Summary
> This program reads a sequential file (CUSTEXTR output), validates each record against reference tables, and inserts/updates the CUSTOMER table. It processes ~500K records nightly with a 2% rejection rate.
>
> ### 🔴 Issues Found
>
> | Issue | Impact | Fix |
> |-------|--------|-----|
> | **Single-row INSERTs** (line 450) | 500K DB2 calls = slow | Batch with `executemany` in Python |
> | **CUSTREF read per record** (line 320) | N+1 pattern, 500K reads | Cache reference table in memory |
> | **No checkpoint/restart** | Full rerun on failure | Add RUN_ID + commit points |
> | **Hardcoded date '2020-01-01'** (line 280) | Time bomb | Make configurable |
>
> ### Business Rules Extracted
> 1. Customer status must be 'A' (Active) or 'P' (Pending) to load
> 2. Balance must be >= 0 (negative balances rejected)
> 3. Customer type '99' triggers special processing (line 380-420)
>
> ### Recommendation for Python Version
> - Use chunked reading + multiprocessing for parsing
> - Cache CUSTREF table at startup (~10K rows fits in memory)
> - Batch DB inserts with 5000-record batches
> - Add RUN_ID and checkpoint every 50K records

---

## ✅ Checklist Before Handoff

Before handing off to `@Python-Batch-Expert`, verify:

- [ ] `modernization-spec.md` exists and is complete
- [ ] All business rules are numbered and testable in `business-rules.md`
- [ ] Copybook field layouts include offset, length, and Python type mapping
- [ ] Data flow diagram shows all inputs/outputs
- [ ] Improvement suggestions include rationale and Python implementation hints
- [ ] All hardcoded values are documented with recommended externalization
- [ ] Error handling paths are fully documented

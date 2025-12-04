# Phase 2: Project Structure & Domain Models

> **Use Case**: Create the Python project structure and generate domain models (@dataclass) from COBOL copybooks.

---

## Prompt

```
@workspace Based on the analysis of [JOB_NAME], create:

1. Python project structure in python/[job_name]/ per 00-core-architecture.md
   - Include pipeline/ folder for producer-consumer pattern
2. Domain models (@dataclass) from all copybooks in source/jobs/[JOB_NAME]/copybooks/
3. Handle all COMP-3 packed decimal fields properly
4. Include validation rules based on field definitions

Generate the src/core/models.py and src/core/validators.py files.
```

---

## Single Copybook Conversion

```
@workspace Convert the copybook source/jobs/[JOB_NAME]/copybooks/[COPYBOOK].cpy 
to a Python @dataclass. Handle all COMP-3 fields and include field validation.
```

---

## Expected Output

### models.py
```python
from dataclasses import dataclass, field
from datetime import date
from decimal import Decimal
from typing import List

@dataclass
class CustomerRecord:
    cust_id: str              # PIC X(10)
    cust_name: str            # PIC X(50)
    balance: Decimal          # PIC S9(9)V99 COMP-3
    status: str               # PIC X(1)
    last_update: date         # PIC 9(8) YYYYMMDD
    validation_errors: List[str] = field(default_factory=list)
```

### validators.py
```python
class CustomerValidator:
    def validate(self, record: CustomerRecord) -> List[str]:
        errors = []
        if not record.cust_id.strip():
            errors.append("Customer ID is required")
        if record.balance < Decimal('0'):
            errors.append("Balance cannot be negative")
        return errors
```

---

## Prerequisites
- Run `@COBOL-Reverse-Engineering-Expert` first to generate documentation, or
- Have documentation in `docs/[JOB_NAME]/copybooks/`

---

## Next Steps
Proceed to `03-io-layer.prompt.md` for file I/O implementation

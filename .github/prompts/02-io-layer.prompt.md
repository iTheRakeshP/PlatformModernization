# Phase 2: IO Layer

> **Use Case**: Implement EBCDIC file reading with chunked I/O for 100GB+ files, copybook parsing, and packed decimal conversion.

---

## Prompt

```
@workspace For the [JOB_NAME] modernization, implement the IO layer:

1. FilePartitioner for splitting 100GB+ files by byte ranges
2. PartitionedEBCDICReader with 64KB chunked reading
3. CopybookParser with field definitions from copybooks
4. File writers for any output files
5. Handle all data types: PIC X, PIC 9, COMP-3, COMP

Follow patterns from 01-ebcdic-copybook-handling.instructions.md.
Generate files in python/[job_name]/src/io_layer/
```

---

## Expected Files

```
python/[job_name]/src/io_layer/
├── __init__.py
├── ebcdic_reader.py       # Chunked EBCDIC file reading
├── copybook_parser.py     # Field extraction from raw bytes
├── packed_decimal.py      # COMP-3 conversion utilities
├── file_partitioner.py    # Split large files for multi-reader
└── file_writer.py         # Output file handling (if needed)
```

---

## Key Patterns

### Chunked Reading (64KB buffer)
```python
class EBCDICReader:
    def __init__(self, file_path: str, encoding: str = 'cp037', 
                 record_length: int = None, chunk_size: int = 64 * 1024):
        self._chunk_size = chunk_size  # 64KB read buffer
    
    def read_records(self) -> Iterator[str]:
        buffer = b''
        with open(self._file_path, 'rb') as f:
            while True:
                chunk = f.read(self._chunk_size)  # Read 64KB at once
                if not chunk:
                    break
                # Process chunked data...
```

### COMP-3 Packed Decimal
```python
def unpack_comp3(packed_bytes: bytes, decimal_places: int = 0) -> Decimal:
    # Convert COBOL COMP-3 to Python Decimal
```

---

## Prerequisites
- Domain models from `01-domain-models.prompt.md`
- Copybook field definitions from `docs/[JOB_NAME]/copybooks/`

---

## Next Steps
Proceed to `03-repository.prompt.md` for database layer

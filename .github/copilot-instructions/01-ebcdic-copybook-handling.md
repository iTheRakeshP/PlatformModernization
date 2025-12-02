# EBCDIC & Copybook Handling

## Concept

Mainframe files use EBCDIC encoding (not ASCII/UTF-8) and fixed-width layouts defined by COBOL copybooks. Fields may contain packed decimal (COMP-3) data requiring specialized conversion. This module provides production-ready patterns for reading, parsing, and converting mainframe data formats.

## When to Use

✅ **Use when:**
- Reading files generated on mainframe (z/OS, OS/390)
- Files are in EBCDIC encoding (cp037, cp500, cp1047)
- Layout is defined by COBOL copybook
- Fields contain COMP-3 (packed decimal) data
- Fixed-width or variable-length record formats

❌ **Skip when:**
- Files are already ASCII/UTF-8
- Using CSV/JSON formats
- No COBOL copybook available (use different parsing strategy)

## EBCDIC Code Pages

| Code Page | Region | Use Case |
|-----------|--------|----------|
| `cp037` | USA, Canada, Australia | Most common for North America |
| `cp500` | International (Latin-1) | Western Europe |
| `cp1047` | Open Systems (Latin-1) | Unix System Services on z/OS |
| `cp273` | Germany, Austria | German-speaking countries |
| `cp297` | France | French mainframes |

## Complete Implementation

### 1. Chunked EBCDIC Reader (Optimized for 100GB+ Files)

```python
from typing import Iterator
from pathlib import Path

class EBCDICReader:
    """
    Read EBCDIC-encoded files using chunked I/O for maximum efficiency.
    Optimized for 100GB+ files - uses 64KB read buffers to minimize syscalls.
    
    For 100GB file with 500-byte records:
    - True line-by-line: ~200 million I/O syscalls
    - 64KB chunked: ~1.5 million I/O syscalls (100x fewer!)
    """
    
    def __init__(self, 
                 file_path: str, 
                 encoding: str = 'cp037',
                 record_length: int = None,
                 chunk_size: int = 64 * 1024,
                 strip_nulls: bool = True):
        """
        Args:
            file_path: Path to EBCDIC file
            encoding: EBCDIC code page (cp037, cp500, cp1047)
            record_length: Fixed record length in bytes (None = variable/line-delimited)
            chunk_size: Read buffer size (default 64KB for optimal I/O)
            strip_nulls: Remove null bytes and trailing whitespace
        """
        self._file_path = Path(file_path)
        self._encoding = encoding
        self._record_length = record_length
        self._chunk_size = chunk_size
        self._strip_nulls = strip_nulls
        
        if not self._file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
    
    def read_records(self) -> Iterator[str]:
        """
        Yield decoded text records using chunked I/O (streaming).
        Memory usage is constant regardless of file size.
        
        Yields:
            Decoded string for each record
        """
        if self._record_length:
            yield from self._read_fixed_length()
        else:
            yield from self._read_variable_length()
    
    def _read_fixed_length(self) -> Iterator[str]:
        """Read fixed-length records with chunked buffering."""
        buffer = b''
        
        with open(self._file_path, 'rb') as f:
            while True:
                chunk = f.read(self._chunk_size)  # Read 64KB at once
                if not chunk:
                    break
                
                buffer += chunk
                
                # Yield complete records from buffer
                while len(buffer) >= self._record_length:
                    raw_record = buffer[:self._record_length]
                    buffer = buffer[self._record_length:]
                    
                    decoded = raw_record.decode(self._encoding, errors='replace')
                    
                    if self._strip_nulls:
                        decoded = decoded.rstrip('\x00\n\r ')
                    
                    yield decoded
        
        # Validate no partial record remains
        if buffer:
            raise ValueError(
                f"Incomplete record: {len(buffer)} bytes remaining. "
                f"Expected record length: {self._record_length}"
            )
    
    def _read_variable_length(self) -> Iterator[str]:
        """Read variable/line-delimited records (Python's buffered I/O)."""
        with open(self._file_path, 'rb') as f:
            for raw_line in f:  # Python uses ~8KB internal buffer
                decoded = raw_line.decode(self._encoding, errors='replace')
                
                if self._strip_nulls:
                    decoded = decoded.rstrip('\x00\n\r ')
                
                yield decoded
    
    def read_raw_records(self) -> Iterator[bytes]:
        """
        Yield raw bytes for manual parsing (useful with copybooks).
        Uses same chunked I/O pattern.
        """
        if self._record_length:
            buffer = b''
            
            with open(self._file_path, 'rb') as f:
                while True:
                    chunk = f.read(self._chunk_size)
                    if not chunk:
                        break
                    
                    buffer += chunk
                    
                    while len(buffer) >= self._record_length:
                        yield buffer[:self._record_length]
                        buffer = buffer[self._record_length:]
            
            if buffer:
                raise ValueError(f"Incomplete record: {len(buffer)} bytes remaining")
        else:
            with open(self._file_path, 'rb') as f:
                for raw_line in f:
                    yield raw_line
    
    def get_record_count(self) -> int:
        """
        Get total record count (for reconciliation).
        For fixed-length: file_size / record_length
        For variable: count lines (requires full scan)
        """
        file_size = self._file_path.stat().st_size
        
        if self._record_length:
            count = file_size // self._record_length
            remainder = file_size % self._record_length
            if remainder != 0:
                raise ValueError(
                    f"File size {file_size} not divisible by record length {self._record_length}"
                )
            return count
        else:
            # Variable length requires counting
            count = 0
            with open(self._file_path, 'rb') as f:
                for _ in f:
                    count += 1
            return count


# Usage example
reader = EBCDICReader(
    file_path="mainframe_data.dat",
    encoding='cp037',
    record_length=500,      # Fixed 500-byte records
    chunk_size=64 * 1024    # 64KB read buffer
)

batch = []
for record in reader.read_records():
    parsed = copybook_parser.parse(record)
    batch.append(parsed)
    
    if len(batch) >= 5000:
        staging_repo.bulk_insert(batch, run_id)
        batch.clear()
```

**I/O Performance Comparison**:
| File Size | Record Length | Chunked (64KB) | Line-by-line | Improvement |
|-----------|--------------|----------------|--------------|-------------|
| 1 GB | 500 bytes | 16K syscalls | 2M syscalls | 125x |
| 10 GB | 500 bytes | 160K syscalls | 20M syscalls | 125x |
| 100 GB | 500 bytes | 1.6M syscalls | 200M syscalls | 125x |

### 2. Packed Decimal (COMP-3) Conversion

```python
from decimal import Decimal

def unpack_comp3(packed_bytes: bytes, decimal_places: int = 0) -> Decimal:
    """
    Convert COBOL COMP-3 (packed decimal) to Python Decimal.
    
    COMP-3 format:
    - Each byte stores 2 digits (nibbles)
    - Last nibble is sign: C/F = positive, D = negative
    - Example: PIC S9(7)V99 COMP-3 = 5 bytes, decimal_places=2
    
    Args:
        packed_bytes: Raw bytes from COMP-3 field
        decimal_places: Number of decimal places (from COBOL PIC V)
    
    Returns:
        Decimal value with correct sign and scale
    
    Example:
        PIC S9(5)V99 COMP-3 (4 bytes)
        Bytes: 0x01 0x23 0x45 0x6C
        Binary: 00000001 00100011 01000101 01101100
        Nibbles: 0 1 2 3 4 5 6 C (sign)
        Value: 0123456 / 100 = 1234.56
    """
    if not packed_bytes:
        return Decimal('0')
    
    # Extract sign from last nibble
    last_byte = packed_bytes[-1]
    sign_nibble = last_byte & 0x0F
    is_negative = (sign_nibble == 0x0D)  # D = negative, C/F = positive
    
    # Extract digits from all nibbles (except last sign nibble)
    digits = []
    for i, byte_val in enumerate(packed_bytes):
        high_nibble = (byte_val >> 4) & 0x0F
        low_nibble = byte_val & 0x0F
        
        digits.append(str(high_nibble))
        
        # Last byte's low nibble is the sign, not a digit
        if i < len(packed_bytes) - 1:
            digits.append(str(low_nibble))
    
    # Convert to integer
    digit_string = ''.join(digits)
    int_value = int(digit_string)
    
    # Apply decimal scaling
    value = Decimal(int_value) / (10 ** decimal_places)
    
    return -value if is_negative else value


def pack_comp3(value: Decimal, total_digits: int, decimal_places: int = 0) -> bytes:
    """
    Convert Python Decimal to COBOL COMP-3 format.
    Useful for writing files back to mainframe.
    
    Args:
        value: Decimal value to pack
        total_digits: Total digits including decimals (PIC S9(n)V99)
        decimal_places: Decimal precision
    
    Returns:
        Packed bytes
    """
    # Scale to integer
    scaled_value = int(value * (10 ** decimal_places))
    is_negative = scaled_value < 0
    abs_value = abs(scaled_value)
    
    # Convert to digit string with leading zeros
    digit_string = str(abs_value).zfill(total_digits)
    
    # Pack nibbles
    packed = []
    for i in range(0, len(digit_string), 2):
        high_nibble = int(digit_string[i])
        
        if i + 1 < len(digit_string):
            low_nibble = int(digit_string[i + 1])
        else:
            # Last nibble is sign
            low_nibble = 0x0D if is_negative else 0x0C
        
        packed.append((high_nibble << 4) | low_nibble)
    
    # Add sign byte if needed
    if len(digit_string) % 2 == 0:
        packed.append(0x0D if is_negative else 0x0C)
    
    return bytes(packed)
```

### 3. Copybook Field Definition

```python
from dataclasses import dataclass
from typing import Literal

@dataclass
class CopybookField:
    """
    Represents a single field from a COBOL copybook.
    Maps COBOL PIC clauses to Python processing.
    """
    name: str                  # Field name (from COBOL)
    start_pos: int             # 0-based byte offset
    length: int                # Byte length (not character length!)
    data_type: Literal['alphanumeric', 'numeric', 'comp3', 'comp', 'binary']
    decimal_places: int = 0    # For numeric fields with decimal (PIC V)
    is_signed: bool = False    # True if PIC S
    
    @property
    def end_pos(self) -> int:
        """Exclusive end position for slicing."""
        return self.start_pos + self.length


# Example: Define copybook layout
CUSTOMER_COPYBOOK = [
    CopybookField('CUST_ID', 0, 10, 'alphanumeric'),           # PIC X(10)
    CopybookField('ACCT_NUM', 10, 16, 'alphanumeric'),         # PIC X(16)
    CopybookField('BALANCE', 26, 5, 'comp3', decimal_places=2), # PIC S9(7)V99 COMP-3
    CopybookField('OPEN_DATE', 31, 8, 'numeric'),              # PIC 9(8) - YYYYMMDD
    CopybookField('STATUS_CODE', 39, 1, 'alphanumeric'),       # PIC X
    CopybookField('TRANS_COUNT', 40, 4, 'comp'),               # PIC 9(9) COMP (binary)
]
```

### 4. Copybook Parser

```python
from typing import Dict, Any, List
from datetime import datetime, date

class CopybookParser:
    """
    Parse fixed-width records based on COBOL copybook field definitions.
    """
    
    def __init__(self, fields: List[CopybookField], encoding: str = 'cp037'):
        """
        Args:
            fields: List of copybook field definitions
            encoding: EBCDIC encoding for text fields
        """
        self._fields = fields
        self._encoding = encoding
        
        # Validate field definitions
        self._validate_fields()
    
    def _validate_fields(self) -> None:
        """Ensure fields don't overlap and are properly ordered."""
        sorted_fields = sorted(self._fields, key=lambda f: f.start_pos)
        
        for i in range(len(sorted_fields) - 1):
            current = sorted_fields[i]
            next_field = sorted_fields[i + 1]
            
            if current.end_pos > next_field.start_pos:
                raise ValueError(
                    f"Field overlap: {current.name} ends at {current.end_pos}, "
                    f"but {next_field.name} starts at {next_field.start_pos}"
                )
    
    def parse(self, raw_bytes: bytes) -> Dict[str, Any]:
        """
        Parse raw bytes into structured dictionary.
        
        Args:
            raw_bytes: Raw record bytes (EBCDIC encoded)
        
        Returns:
            Dictionary with field names as keys, parsed values
        """
        result = {}
        
        for field in self._fields:
            # Extract field bytes
            field_bytes = raw_bytes[field.start_pos:field.end_pos]
            
            # Parse based on data type
            if field.data_type == 'alphanumeric':
                value = field_bytes.decode(self._encoding, errors='replace').rstrip()
            
            elif field.data_type == 'numeric':
                text = field_bytes.decode(self._encoding, errors='replace').strip()
                value = int(text) if text.isdigit() else 0
            
            elif field.data_type == 'comp3':
                value = unpack_comp3(field_bytes, field.decimal_places)
            
            elif field.data_type == 'comp' or field.data_type == 'binary':
                # Binary integer (big-endian)
                value = int.from_bytes(field_bytes, byteorder='big', signed=field.is_signed)
            
            else:
                raise ValueError(f"Unsupported data type: {field.data_type}")
            
            result[field.name] = value
        
        return result
    
    def parse_to_model(self, raw_bytes: bytes, model_class):
        """
        Parse and instantiate a dataclass model.
        
        Example:
            record = parser.parse_to_model(raw_bytes, CustomerRecord)
        """
        parsed = self.parse(raw_bytes)
        return model_class(**parsed)
```

### 5. Complete Example Usage

```python
from dataclasses import dataclass
from decimal import Decimal
from datetime import date

# 1. Define domain model
@dataclass
class CustomerRecord:
    cust_id: str
    acct_num: str
    balance: Decimal
    open_date: date
    status_code: str
    trans_count: int
    line_no: int = 0

# 2. Setup reader and parser
reader = EBCDICReader(
    file_path='CUSTOMER.DAT',
    encoding='cp037',
    line_length=100  # Fixed 100-byte records
)

parser = CopybookParser(CUSTOMER_COPYBOOK, encoding='cp037')

# 3. Process file
records = []

for line_no, raw_bytes in enumerate(reader.read_bytes(), 1):
    # Parse fields
    parsed = parser.parse(raw_bytes)
    
    # Convert date field (YYYYMMDD)
    open_date = datetime.strptime(str(parsed['OPEN_DATE']), '%Y%m%d').date()
    
    # Create domain model
    record = CustomerRecord(
        cust_id=parsed['CUST_ID'],
        acct_num=parsed['ACCT_NUM'],
        balance=parsed['BALANCE'],
        open_date=open_date,
        status_code=parsed['STATUS_CODE'],
        trans_count=parsed['TRANS_COUNT'],
        line_no=line_no
    )
    
    records.append(record)
    
    # Process in batches
    if len(records) >= 5000:
        # Insert to database
        staging_repo.bulk_insert(records, run_id)
        records.clear()

# Insert remaining
if records:
    staging_repo.bulk_insert(records, run_id)
```

## Configuration

```yaml
# config/base_config.yaml
file_processing:
  encoding: "cp037"          # EBCDIC code page
  line_length: 500           # Fixed record length (bytes)
  strip_nulls: true          # Remove trailing nulls/spaces
  error_handling: "replace"  # replace | ignore | strict
  
  # Copybook location
  copybook_dir: "config/copybooks"
  copybook_file: "CUSTOMER.cpy"
```

## Common Pitfalls

❌ **Wrong encoding**
```python
# Bad - assumes UTF-8
text = bytes.decode('utf-8')  # UnicodeDecodeError on EBCDIC
```

✅ **Specify EBCDIC code page**
```python
# Good
text = bytes.decode('cp037')  # Correct for US mainframes
```

❌ **Using character length for byte slicing**
```python
# Bad - COMP-3 field is 5 bytes, not 9 characters
field_bytes = raw_bytes[10:19]  # Wrong!
```

✅ **Use byte offsets from copybook**
```python
# Good
field_bytes = raw_bytes[10:15]  # 5 bytes for PIC S9(7)V99 COMP-3
```

❌ **Float for packed decimal**
```python
# Bad - loses precision
balance = float(unpack_comp3(bytes))
```

✅ **Decimal for currency**
```python
# Good - exact precision
balance = unpack_comp3(bytes, decimal_places=2)  # Returns Decimal
```

❌ **Loading entire file**
```python
# Bad - OOM on large files
all_lines = open(file, 'rb').read().splitlines()
```

✅ **Stream line-by-line**
```python
# Good - constant memory
for line in reader.read_lines():
    process(line)
```

## File Partitioner for Parallel Processing

For 100GB+ files, split into partitions for parallel processing by multiple reader workers:

```python
import os
from typing import List, Tuple
from dataclasses import dataclass


@dataclass
class FilePartition:
    """Represents a byte-range partition of a file."""
    partition_id: int
    start_byte: int
    end_byte: int
    
    @property
    def size_bytes(self) -> int:
        return self.end_byte - self.start_byte


class FileSplitter:
    """
    Split large files into separate chunk files for multi-reader processing.
    
    IMPORTANT: For multi-reader mode, files MUST be physically split into 
    separate files. Multiple processes reading the same file causes contention.
    
    This class creates actual chunk files that each reader can open exclusively.
    """
    
    def __init__(self, record_length: int, 
                 target_chunk_size: int = 25 * 1024**3):
        """
        Args:
            record_length: Fixed record length in bytes (from copybook)
            target_chunk_size: Target size per chunk file (default 25GB)
        """
        self.record_length = record_length
        self.target_chunk_size = target_chunk_size
    
    def split(self, input_file: str, output_dir: str) -> List[str]:
        """
        Split file into separate chunk files for multi-reader processing.
        
        Args:
            input_file: Path to large input file
            output_dir: Directory to write chunk files
        
        Returns:
            List of chunk file paths (one per reader)
        
        Example:
            100GB file → 4 chunk files of ~25GB each
            Each reader opens its own chunk file (no contention)
        """
        Path(output_dir).mkdir(parents=True, exist_ok=True)
        
        # Align chunk size to record boundary
        records_per_chunk = self.target_chunk_size // self.record_length
        bytes_per_chunk = records_per_chunk * self.record_length
        
        chunk_files = []
        chunk_num = 0
        
        with open(input_file, 'rb') as src:
            while True:
                chunk_data = src.read(bytes_per_chunk)
                if not chunk_data:
                    break
                
                # Validate we have complete records
                if len(chunk_data) % self.record_length != 0:
                    # Only allow partial at end of file
                    if src.read(1):  # More data exists
                        raise ValueError(
                            f"Chunk {chunk_num} has incomplete record "
                            f"({len(chunk_data)} bytes, record_length={self.record_length})"
                        )
                
                chunk_path = Path(output_dir) / f"chunk_{chunk_num:04d}.dat"
                with open(chunk_path, 'wb') as dst:
                    dst.write(chunk_data)
                
                chunk_files.append(str(chunk_path))
                chunk_num += 1
        
        return chunk_files
    
    def estimate_chunks(self, input_file: str) -> int:
        """Calculate how many chunks will be created."""
        file_size = os.path.getsize(input_file)
        records_per_chunk = self.target_chunk_size // self.record_length
        bytes_per_chunk = records_per_chunk * self.record_length
        return (file_size + bytes_per_chunk - 1) // bytes_per_chunk


class FileSplitValidator:
    """Validate that chunk files are ready for multi-reader processing."""
    
    @staticmethod
    def validate_chunks(chunk_files: List[str], record_length: int) -> bool:
        """Verify all chunks are aligned to record boundaries."""
        for chunk_file in chunk_files:
            size = os.path.getsize(chunk_file)
            if size % record_length != 0:
                raise ValueError(
                    f"Chunk {chunk_file} has size {size} bytes, "
                    f"not aligned to record length {record_length}"
                )
        return True
    
    @staticmethod
    def find_existing_chunks(pattern: str) -> List[str]:
        """
        Find existing chunk files matching glob pattern.
        
        Args:
            pattern: Glob pattern like "chunks/chunk_*.dat"
        
        Returns:
            Sorted list of chunk file paths
        """
        from glob import glob
        chunks = sorted(glob(pattern))
        if not chunks:
            raise FileNotFoundError(f"No chunk files found: {pattern}")
        return chunks


class ChunkFileReader(EBCDICReader):
    """
    EBCDIC Reader for a single chunk file (multi-reader mode).
    
    Each ChunkFileReader instance has exclusive access to its chunk file.
    No file contention between readers.
    """
    
    def __init__(self, file_path: str, encoding: str = 'cp037', 
                 record_length: int = None, chunk_size: int = 64 * 1024):
        """
        Args:
            file_path: Path to this reader's EXCLUSIVE chunk file
            encoding: EBCDIC code page
            record_length: Fixed record length
            chunk_size: Read buffer size (64KB)
        """
        super().__init__(file_path, encoding, record_length, chunk_size)
    
    # Inherits read_records() from EBCDICReader - reads entire chunk file


# Usage Examples
def example_single_reader_mode():
    """Single reader mode - one process reads entire file."""
    reader = EBCDICReader(
        file_path='/data/mainframe.dat',
        encoding='cp037',
        record_length=500
    )
    
    for record in reader.read_records():
        # Process record
        pass


def example_multi_reader_mode():
    """Multi-reader mode - split file first, then each reader gets own file."""
    
    # Step 1: Split file into chunks (run once, before pipeline)
    splitter = FileSplitter(record_length=500, target_chunk_size=25 * 1024**3)
    chunk_files = splitter.split('/data/mainframe.dat', '/tmp/chunks')
    # Creates: /tmp/chunks/chunk_0000.dat, chunk_0001.dat, etc.
    
    # Step 2: Each reader processes its own chunk file
    for chunk_file in chunk_files:
        reader = ChunkFileReader(
            file_path=chunk_file,  # Exclusive file!
            encoding='cp037',
            record_length=500
        )
        for record in reader.read_records():
            pass


def example_use_existing_chunks():
    """Use pre-existing chunk files (already split)."""
    
    chunk_files = FileSplitValidator.find_existing_chunks(
        '/data/chunks/chunk_*.dat'
    )
    FileSplitValidator.validate_chunks(chunk_files, record_length=500)
    
    for chunk_file in chunk_files:
        reader = ChunkFileReader(chunk_file, record_length=500)
        # Process...
```

## Quick Reference

| COBOL Type | Python Parsing |
|------------|----------------|
| `PIC X(n)` | `bytes.decode(encoding).rstrip()` |
| `PIC 9(n)` | `int(bytes.decode(encoding))` |
| `PIC S9(n)V99 COMP-3` | `unpack_comp3(bytes, decimal_places=2)` |
| `PIC 9(n) COMP` | `int.from_bytes(bytes, 'big')` |
| `PIC S9(n) COMP` | `int.from_bytes(bytes, 'big', signed=True)` |
| `PIC 9(8)` (date) | `datetime.strptime(str(val), '%Y%m%d')` |
| `PIC 9(6)` (time) | `datetime.strptime(str(val), '%H%M%S')` |

## Testing

```python
import pytest
from decimal import Decimal

def test_unpack_comp3_positive():
    # PIC S9(5)V99 COMP-3: value 1234.56
    # Bytes: 0x01 0x23 0x45 0x6C (C = positive)
    packed = bytes([0x01, 0x23, 0x45, 0x6C])
    result = unpack_comp3(packed, decimal_places=2)
    assert result == Decimal('1234.56')

def test_unpack_comp3_negative():
    # PIC S9(5)V99 COMP-3: value -1234.56
    # Bytes: 0x01 0x23 0x45 0x6D (D = negative)
    packed = bytes([0x01, 0x23, 0x45, 0x6D])
    result = unpack_comp3(packed, decimal_places=2)
    assert result == Decimal('-1234.56')

def test_ebcdic_reader_fixed_length():
    # Create test file with EBCDIC data
    test_data = "CUST001   ".encode('cp037') + b'\x00' * 90
    with open('test.dat', 'wb') as f:
        f.write(test_data)
    
    reader = EBCDICReader('test.dat', encoding='cp037', line_length=100)
    lines = list(reader.read_lines())
    
    assert len(lines) == 1
    assert lines[0].startswith('CUST001')

def test_file_partitioner():
    # 100 records × 500 bytes = 50KB file, 4 partitions
    partitions = FilePartitioner.partition_fixed_length(
        'test_large.dat', 
        num_partitions=4, 
        record_length=500
    )
    
    assert len(partitions) == 4
    # Each partition should get ~25 records (12,500 bytes)
    assert partitions[0].start_byte == 0
    assert partitions[3].end_byte == 50000
```

## Next Steps

- **Pipeline Integration**: See `00-core-architecture.md` for ReaderProcessor pattern
- **Repository Pattern**: See `02-repository-pattern.md` for DB2 integration
- **Validation**: Implement business rules after parsing
- **Error Handling**: Write invalid records to error file



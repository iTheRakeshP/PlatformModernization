# Phase 8: Testing

> **Use Case**: Generate comprehensive unit and integration tests for the modernized batch job.

---

## Prompt

```
@workspace For the [JOB_NAME] modernization, create tests:

1. Unit tests for domain models and validators
2. Unit tests for IO layer with mock EBCDIC data
3. Unit tests for repository layer with mocked connections
4. Integration test outline for full flow
5. Test fixtures with sample data

Generate files in python/[job_name]/tests/
```

---

## Expected Files

```
python/[job_name]/tests/
├── __init__.py
├── conftest.py                    # Shared pytest fixtures
├── unit/
│   ├── __init__.py
│   ├── test_models.py             # Domain model tests (entity/)
│   ├── test_validators.py         # Validation logic tests (processor/)
│   ├── test_ebcdic_reader.py      # IO layer tests
│   ├── test_copybook_parser.py    # Parser tests
│   ├── test_packed_decimal.py     # COMP-3 conversion tests
│   └── test_repository.py         # Repository tests (mocked)
├── integration/
│   ├── __init__.py
│   └── test_pipeline.py           # Full flow tests
└── fixtures/
    ├── sample_input.dat           # Sample EBCDIC data
    └── expected_output.json       # Expected results
```

---

## Key Test Patterns

### Unit Test with Mocks
```python
@pytest.fixture
def mock_connection_factory():
    factory = Mock()
    conn = MagicMock()
    cursor = MagicMock()
    factory.create.return_value = conn
    conn.cursor.return_value = cursor
    return factory, conn, cursor

def test_bulk_insert(mock_connection_factory):
    factory, conn, cursor = mock_connection_factory
    repository = StagingRepository(factory, batch_size=2)
    
    records = [
        CustomerRecord(cust_id='ACCT001', ...),
        CustomerRecord(cust_id='ACCT002', ...),
    ]
    
    count = repository.bulk_insert(records, 'TEST_RUN_001')
    
    assert count == 2
    assert cursor.executemany.call_count == 1
```

### COMP-3 Conversion Test
```python
def test_unpack_comp3_positive():
    # PIC S9(5)V99 COMP-3 = 12345.67 positive
    packed = bytes([0x01, 0x23, 0x45, 0x67, 0x0C])
    result = unpack_comp3(packed, decimal_places=2)
    assert result == Decimal('12345.67')

def test_unpack_comp3_negative():
    # PIC S9(5)V99 COMP-3 = -12345.67 negative
    packed = bytes([0x01, 0x23, 0x45, 0x67, 0x0D])
    result = unpack_comp3(packed, decimal_places=2)
    assert result == Decimal('-12345.67')
```

### Integration Test Marker
```python
@pytest.mark.integration
def test_end_to_end_pipeline(db2_connection, sample_input_file):
    """Full pipeline test - requires DB2 connection."""
    run_id = 'TEST_E2E_001'
    
    orchestrator = PipelineOrchestrator(config)
    result = orchestrator.run(sample_input_file, run_id)
    
    assert result.status == 'SUCCESS'
    assert result.records_processed > 0
    assert result.reconciliation_passed
```

---

## Running Tests

```bash
# Unit tests only
pytest tests/unit -v

# Integration tests (requires DB2)
pytest tests/integration -v --db2-available

# With coverage
pytest tests/unit --cov=src --cov-report=html
```

---

## Prerequisites
- All previous phases complete
- Sample test data available

---

## Next Steps
- Run tests to validate implementation
- Use `10-troubleshooting.prompt.md` if issues arise

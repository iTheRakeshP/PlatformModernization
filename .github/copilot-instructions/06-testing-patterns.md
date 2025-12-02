# Testing Patterns

Unit tests (70%), integration tests (20%), E2E tests (10%). Mock DB connections, use pytest fixtures, test with real DB2 for integration layer.

## Unit Test Pattern
```python
@pytest.fixture
def mock_pool():
    pool = Mock()
    conn = MagicMock()
    pool.get_connection.return_value.connection = conn
    return pool, conn

def test_bulk_insert(mock_pool):
    pool, conn = mock_pool
    repo = Db2StagingRepository(pool)
    count = repo.bulk_insert(records, 'RUN_001')
    assert count == len(records)
```

Reference: Section 10

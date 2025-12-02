# Security & Compliance

PII masking, encryption at rest/in transit, audit trails, least-privilege DB access, secrets in vault.

## PII Masking Pattern
```python
class PIIMasker:
    @staticmethod
    def mask_account_id(acct_id: str) -> str:
        if len(acct_id) <= 4:
            return '****'
        return '*' * (len(acct_id) - 4) + acct_id[-4:]
```

Reference: Section 11

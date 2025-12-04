````prompt
# Phase 8: Security & Compliance

> **Use Case**: Implement security controls including PII masking in logs, encryption at rest, secrets management, and audit trail requirements.

---

## Prompt

```
@workspace For the [JOB_NAME] modernization, implement security controls:

1. PII masking in all log output:
   - Mask account numbers, SSNs, names in logs
   - Use regex patterns for common PII fields
   - Configure masking rules in YAML
2. Secrets management:
   - HashiCorp Vault integration for DB credentials
   - Environment variable fallback for local dev
   - Never log or expose secrets
3. Encryption at rest:
   - Encrypt sensitive fields before staging table insert
   - Use Fernet symmetric encryption
   - Key rotation support
4. Audit trail:
   - Log all data access with user/process identity
   - Track record-level changes where required
   - Compliance with retention policies
5. Input validation:
   - Sanitize all external inputs
   - Prevent injection attacks in SQL operations

Follow patterns from 07-security-compliance.instructions.md.
Generate python/[job_name]/src/utility/security.py and update logging.
```

---

## Expected Files

```
python/[job_name]/src/utility/
├── security.py              # PII masking, encryption utilities
├── secrets_manager.py       # Vault integration
└── audit_logger.py          # Compliance audit logging
```

---

## Key Patterns

### PII Masking in Logs
```python
import re
from typing import Dict, Pattern

class PIIMasker:
    """Mask sensitive data in log messages."""
    
    def __init__(self, patterns: Dict[str, str]):
        self._patterns: Dict[str, Pattern] = {
            name: re.compile(pattern, re.IGNORECASE)
            for name, pattern in patterns.items()
        }
    
    def mask(self, message: str) -> str:
        """Apply all masking patterns to message."""
        result = message
        for name, pattern in self._patterns.items():
            result = pattern.sub(f'[{name.upper()}_MASKED]', result)
        return result

# Default patterns
DEFAULT_PII_PATTERNS = {
    'ssn': r'\b\d{3}-\d{2}-\d{4}\b',
    'account': r'\b\d{10,16}\b',
    'email': r'\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b',
}
```

### Secrets Manager with Vault
```python
import os
from typing import Optional

class SecretsManager:
    """Retrieve secrets from Vault or environment."""
    
    def __init__(self, vault_client=None):
        self._vault = vault_client
    
    def get_secret(self, key: str, path: str = 'secret/data/batch') -> Optional[str]:
        """Get secret from Vault, fall back to environment."""
        # Try Vault first
        if self._vault:
            try:
                secret = self._vault.secrets.kv.v2.read_secret_version(
                    path=path, mount_point='secret'
                )
                return secret['data']['data'].get(key)
            except Exception:
                pass
        
        # Fall back to environment variable
        return os.environ.get(key.upper())
```

### Field-Level Encryption
```python
from cryptography.fernet import Fernet
from typing import Optional

class FieldEncryptor:
    """Encrypt/decrypt sensitive fields."""
    
    def __init__(self, key: bytes):
        self._fernet = Fernet(key)
    
    def encrypt(self, value: str) -> str:
        """Encrypt a string value."""
        return self._fernet.encrypt(value.encode()).decode()
    
    def decrypt(self, encrypted: str) -> str:
        """Decrypt an encrypted value."""
        return self._fernet.decrypt(encrypted.encode()).decode()
```

### Audit Logger
```python
import logging
from datetime import datetime
from typing import Any, Dict

class AuditLogger:
    """Log audit events for compliance."""
    
    def __init__(self, run_id: str, logger: logging.Logger):
        self._run_id = run_id
        self._logger = logger
    
    def log_access(self, table: str, operation: str, 
                   record_count: int, user: str = None) -> None:
        """Log data access event."""
        self._logger.info(
            "AUDIT_ACCESS",
            extra={
                'run_id': self._run_id,
                'timestamp': datetime.utcnow().isoformat(),
                'table': table,
                'operation': operation,
                'record_count': record_count,
                'user': user or 'SYSTEM',
            }
        )
```

---

## Configuration

### security_config.yaml
```yaml
security:
  pii_masking:
    enabled: true
    patterns:
      ssn: '\b\d{3}-\d{2}-\d{4}\b'
      account: '\b\d{10,16}\b'
      phone: '\b\d{3}[-.]?\d{3}[-.]?\d{4}\b'
  
  encryption:
    enabled: true
    algorithm: "fernet"
    key_source: "vault"  # or "env"
    key_rotation_days: 90
  
  vault:
    address: "https://vault.company.com:8200"
    auth_method: "approle"
    secret_path: "secret/data/batch"
  
  audit:
    enabled: true
    log_level: "INFO"
    retention_days: 365
```

---

## Prerequisites
- All previous phases complete
- Vault instance available (for production)
- Encryption keys generated

---

## Next Steps
- Proceed to `12-performance-tuning.prompt.md` for optimization
- Use `09-troubleshooting.prompt.md` if issues arise

````

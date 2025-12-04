# Dependency Management Prompts

> **Use Case**: Generate and verify requirements.txt files for modernized Python projects.

---

## Generate requirements.txt

```
@workspace Scan python/[job_name]/src/ for all imports and generate:

1. requirements.txt with all production dependencies:
   - Pin versions with >= (e.g., ibm-db>=3.1.0)
   - Group by category (Core, DB, Config, Utilities)
   - Include comments for non-obvious packages

2. requirements-dev.txt with development/test dependencies:
   - pytest, pytest-cov, pytest-mock
   - Any testing utilities

Ensure every import in the code has a corresponding entry.
```

---

## Verify Dependencies

```
@workspace Check python/[job_name]/ for missing dependencies:
1. Scan all .py files for import statements
2. Compare against requirements.txt
3. Report any missing packages
4. Suggest version pins based on current stable releases
```

---

## Update Dependencies

```
@workspace Update python/[job_name]/requirements.txt:
1. Check for outdated packages
2. Suggest version updates (maintaining compatibility)
3. Note any breaking changes in major versions
4. Ensure all security patches are applied
```

---

## Expected requirements.txt Format

```text
# ============================================
# Production Dependencies for [JOB_NAME] Batch
# ============================================

# Core
python-dateutil>=2.8.0

# DB2 Connectivity
ibm-db>=3.1.0
ibm-db-dbi>=3.0.0

# Configuration
pyyaml>=6.0
jsonschema>=4.0.0

# Secrets Management (optional)
hvac>=1.0.0  # HashiCorp Vault client

# Security
cryptography>=41.0.0
```

---

## Expected requirements-dev.txt Format

```text
# ============================================
# Development/Test Dependencies
# ============================================

# Testing
pytest>=7.4.0
pytest-cov>=4.1.0
pytest-mock>=3.11.0

# Code Quality
black>=23.0.0
isort>=5.12.0
mypy>=1.5.0

# Type Stubs
types-PyYAML>=6.0.0
```

---

## Audit Dependencies

```
@workspace Audit python/[job_name]/requirements.txt for security vulnerabilities:
1. Check against known CVE databases
2. Identify packages with security advisories
3. Suggest safe upgrade paths
4. Note any packages that need immediate attention
```

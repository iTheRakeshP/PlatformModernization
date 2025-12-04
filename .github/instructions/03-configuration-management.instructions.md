# Configuration Management

## Concept

Multi-environment configuration system supporting dev/test/prod environments with secrets management via HashiCorp Vault or CyberArk. Uses YAML for structured configuration with environment variable overrides and schema validation.

## When to Use

✅ **Use when:**
- Multiple environments (dev, test, prod)
- Secrets must not be in source control
- Need environment-specific settings (DB connections, file paths)
- Configuration changes without code changes
- Vault integration required

❌ **Consider simpler approach when:**
- Single environment only
- No sensitive data
- Very simple configuration (few settings)

## Complete Implementation

### 1. Configuration Loader

```python
from pathlib import Path
from typing import Dict, Any, Optional
import yaml
import os
import logging
from copy import deepcopy

logger = logging.getLogger(__name__)


class ConfigLoader:
    """
    Multi-environment configuration loader.
    Loads base config, merges environment-specific overrides, applies vault secrets.
    """
    
    def __init__(self, 
                 config_dir: str,
                 environment: str = "dev",
                 enable_vault: bool = False):
        """
        Args:
            config_dir: Path to configuration directory
            environment: Environment name (dev, test, prod)
            enable_vault: Enable vault integration for secrets
        """
        self._config_dir = Path(config_dir)
        self._environment = environment
        self._enable_vault = enable_vault
        self._config: Dict[str, Any] = {}
        
        if not self._config_dir.exists():
            raise FileNotFoundError(f"Config directory not found: {config_dir}")
    
    def load(self) -> Dict[str, Any]:
        """
        Load configuration in order:
        1. Base configuration (base_config.yaml)
        2. Environment-specific overrides ({env}_config.yaml)
        3. Secrets from vault or file (secrets/{env}_secrets.yaml)
        4. Environment variable overrides
        
        Returns:
            Complete merged configuration dictionary
        """
        # Step 1: Load base configuration
        base_file = self._config_dir / "base_config.yaml"
        if base_file.exists():
            self._config = self._load_yaml(base_file)
            logger.info(f"Loaded base configuration from {base_file}")
        else:
            logger.warning(f"Base config not found: {base_file}")
            self._config = {}
        
        # Step 2: Merge environment-specific configuration
        env_file = self._config_dir / f"{self._environment}_config.yaml"
        if env_file.exists():
            env_config = self._load_yaml(env_file)
            self._deep_merge(self._config, env_config)
            logger.info(f"Merged {self._environment} configuration from {env_file}")
        else:
            logger.warning(f"Environment config not found: {env_file}")
        
        # Step 3: Load secrets
        if self._enable_vault:
            secrets = self._load_from_vault()
        else:
            secrets = self._load_secrets_from_file()
        
        if secrets:
            self._deep_merge(self._config, secrets)
            logger.info("Merged secrets into configuration")
        
        # Step 4: Apply environment variable overrides
        self._apply_env_var_overrides()
        
        # Step 5: Validate configuration
        self._validate_config()
        
        return self._config
    
    def _load_yaml(self, file_path: Path) -> Dict[str, Any]:
        """Load YAML file safely."""
        try:
            with open(file_path, 'r') as f:
                return yaml.safe_load(f) or {}
        except Exception as e:
            logger.error(f"Failed to load YAML from {file_path}: {e}")
            raise
    
    def _load_secrets_from_file(self) -> Dict[str, Any]:
        """Load secrets from local file (dev/test environments)."""
        secrets_dir = self._config_dir / "secrets"
        secrets_file = secrets_dir / f"{self._environment}_secrets.yaml"
        
        if secrets_file.exists():
            return self._load_yaml(secrets_file)
        else:
            logger.warning(f"Secrets file not found: {secrets_file}")
            return {}
    
    def _load_from_vault(self) -> Dict[str, Any]:
        """Load secrets from HashiCorp Vault."""
        try:
            vault_config = self._config.get('vault', {})
            if not vault_config.get('enabled'):
                return {}
            
            import hvac  # HashiCorp Vault client
            
            client = hvac.Client(
                url=vault_config['url'],
                token=os.getenv('VAULT_TOKEN')
            )
            
            if not client.is_authenticated():
                raise Exception("Vault authentication failed")
            
            # Read secret from vault
            secret_path = vault_config['secret_path']
            response = client.secrets.kv.v2.read_secret_version(path=secret_path)
            
            secrets = response['data']['data']
            logger.info(f"Loaded secrets from vault: {secret_path}")
            return secrets
            
        except Exception as e:
            logger.error(f"Failed to load secrets from vault: {e}")
            raise
    
    def _deep_merge(self, base: Dict, override: Dict) -> None:
        """
        Deep merge override dict into base dict.
        Modifies base in-place.
        """
        for key, value in override.items():
            if key in base and isinstance(base[key], dict) and isinstance(value, dict):
                # Recursively merge nested dicts
                self._deep_merge(base[key], value)
            else:
                # Override value
                base[key] = value
    
    def _apply_env_var_overrides(self) -> None:
        """
        Apply environment variable overrides.
        Format: BATCH_DB2_CONNECTION_POOL_MAX_SIZE=20
        Converts to: config['db2']['connection_pool']['max_size'] = 20
        """
        prefix = "BATCH_"
        
        for env_var, value in os.environ.items():
            if not env_var.startswith(prefix):
                continue
            
            # Remove prefix and convert to lowercase path
            path = env_var[len(prefix):].lower().split('_')
            
            # Navigate config dict and set value
            current = self._config
            for key in path[:-1]:
                if key not in current:
                    current[key] = {}
                current = current[key]
            
            # Convert value to appropriate type
            final_key = path[-1]
            current[final_key] = self._convert_value(value)
            
            logger.debug(f"Applied env var override: {env_var} = {value}")
    
    def _convert_value(self, value: str) -> Any:
        """Convert string value to appropriate Python type."""
        # Boolean
        if value.lower() in ('true', 'yes', '1'):
            return True
        if value.lower() in ('false', 'no', '0'):
            return False
        
        # Integer
        try:
            return int(value)
        except ValueError:
            pass
        
        # Float
        try:
            return float(value)
        except ValueError:
            pass
        
        # String (default)
        return value
    
    def _validate_config(self) -> None:
        """Validate required configuration keys exist."""
        required_keys = [
            ('application', 'name'),
            ('db2', 'connection', 'database'),
            ('db2', 'connection', 'hostname'),
        ]
        
        for key_path in required_keys:
            current = self._config
            for key in key_path:
                if key not in current:
                    raise ValueError(f"Missing required config: {'.'.join(key_path)}")
                current = current[key]
    
    def get(self, *keys: str, default: Any = None) -> Any:
        """
        Get nested configuration value.
        
        Example:
            config.get('db2', 'connection_pool', 'max_size')
        """
        current = self._config
        for key in keys:
            if isinstance(current, dict) and key in current:
                current = current[key]
            else:
                return default
        return current
    
    def get_db2_config(self) -> Dict[str, Any]:
        """Get DB2 configuration section."""
        return self._config.get('db2', {})
    
    def get_batch_config(self) -> Dict[str, Any]:
        """Get batch processing configuration."""
        return self._config.get('batch', {})
```

### 2. Configuration Files

#### base_config.yaml
```yaml
# Base configuration - shared across all environments

application:
  name: "MainframeBatchModernization"
  version: "1.0.0"
  log_level: "INFO"

# Pipeline configuration (producer-consumer pattern)
pipeline:
  # Reading mode selection
  reading_mode: "single"      # "single" (default) or "multi"
  
  # Single reader settings (reading_mode: "single")
  single_reader:
    chunk_size: 65536         # 64KB read buffer
  
  # Multi-reader settings (reading_mode: "multi")
  # REQUIRES pre-split files - see FileSplitter in 01-ebcdic-copybook-handling.md
  multi_reader:
    # Option 1: Use existing pre-split chunk files
    input_pattern: null       # e.g., "chunks/chunk_*.dat"
    
    # Option 2: Split file before processing
    split_before_processing: false
    split_output_dir: "/tmp/chunks"
    chunk_size_gb: 25         # Target size per chunk file
  
  # Processor workers (for single-reader mode)
  num_processors: 4           # Parallel processing workers
  
  # DB Writer workers
  num_writers: 2              # Parallel DB writers
  
  # Bounded queues (backpressure control)
  raw_queue_max_size: 20      # Reader → Processors (single mode)
  processed_queue_max_size: 10  # Processors → Writers
  queue_timeout: 30           # Seconds to wait for queue operations
  
  # Batch settings
  batch_size: 5000            # Records per batch

batch:
  default_batch_size: 5000
  max_error_threshold: 100
  max_retries: 3

file_processing:
  encoding: "cp037"           # EBCDIC code page
  record_length: null         # null = variable length, set for fixed-width
  strip_nulls: true
  error_file_suffix: "_errors"

db2:
  driver: "ibm_db_dbi"
  
  # Connection settings (per DB Writer)
  connection:
    database: ""              # Set in environment config
    hostname: ""
    port: 50000
    protocol: "TCPIP"
    security: null            # "SSL" or null
  
  repository:
    staging_table: "STG_BATCH"
    final_table: "FINAL_BATCH"
    control_table: "BATCH_CONTROL"
    recon_table: "RECON_AUDIT"
  
  retry:
    max_attempts: 3
    backoff_factor: 2.0
    initial_delay: 1.0
    max_delay: 30.0

reconciliation:
  enabled: true
  count_check: true
  hash_validation: false
  fail_on_mismatch: true

logging:
  format: "json"              # json | text
  output: "file"              # file | console | both
  file_path: "logs/batch_{date}.log"
  rotation: "daily"
  retention_days: 30

monitoring:
  enabled: true
  metrics_interval: 60        # seconds
  export_format: "prometheus"
```

#### dev_config.yaml
```yaml
# Development environment overrides

application:
  log_level: "DEBUG"

pipeline:
  reading_mode: "single"      # Single reader for dev simplicity
  num_processors: 2           # Less parallelism in dev
  num_writers: 1
  batch_size: 100             # Smaller batches for testing

db2:
  connection:
    database: "DEV_DB"
    hostname: "dev-db2.company.com"
    port: 50000
    protocol: "TCPIP"

logging:
  output: "console"           # Console logging in dev
```

#### prod_config.yaml
```yaml
# Production environment overrides

application:
  log_level: "INFO"

pipeline:
  # Choose reading mode based on storage type:
  # - "single": Network storage (NFS/SAN), variable-length records
  # - "multi": Local SSD with pre-split files
  reading_mode: "single"
  
  # Single reader settings
  single_reader:
    chunk_size: 131072        # 128KB for fast storage
  
  # If using multi-reader mode, configure:
  # multi_reader:
  #   input_pattern: "/data/chunks/chunk_*.dat"
  
  num_processors: 8           # Scale up for production
  num_writers: 4
  batch_size: 5000            # Larger batches for throughput
  processed_queue_max_size: 20  # Larger queue for production

batch:
  max_error_threshold: 1000

db2:
  connection:
    database: "PROD_DB"
    hostname: "prod-db2.company.com"
    port: 50000
    protocol: "TCPIP"
    security: "SSL"

vault:
  enabled: true
  url: "https://vault.company.com"
  secret_path: "secret/data/batch/prod"

logging:
  output: "file"
  retention_days: 90
```

#### secrets/prod_secrets.yaml (NEVER commit to git)
```yaml
# Production secrets - DO NOT COMMIT
# Add to .gitignore

db2:
  connection:
    uid: "prod_batch_user"
    pwd: "SecurePassword123!"

vault:
  token: "s.abc123xyz789"

encryption:
  key: "32-byte-encryption-key-here-123"
  salt: "16-byte-salt-val"
```

### 3. .gitignore Entry
```
# Configuration secrets
config/secrets/
*_secrets.yaml
*.key
*.pem
```

### 4. Schema Validation (Optional)

```python
from jsonschema import validate, ValidationError

CONFIG_SCHEMA = {
    "type": "object",
    "required": ["application", "db2", "batch"],
    "properties": {
        "application": {
            "type": "object",
            "required": ["name"],
            "properties": {
                "name": {"type": "string"},
                "log_level": {"type": "string", "enum": ["DEBUG", "INFO", "WARNING", "ERROR"]}
            }
        },
        "db2": {
            "type": "object",
            "required": ["connection"],
            "properties": {
                "connection": {
                    "type": "object",
                    "required": ["database", "hostname", "port"],
                    "properties": {
                        "database": {"type": "string"},
                        "hostname": {"type": "string"},
                        "port": {"type": "integer", "minimum": 1, "maximum": 65535}
                    }
                }
            }
        }
    }
}

def validate_config(config: Dict[str, Any]) -> None:
    """Validate configuration against schema."""
    try:
        validate(instance=config, schema=CONFIG_SCHEMA)
        logger.info("Configuration validation passed")
    except ValidationError as e:
        logger.error(f"Configuration validation failed: {e.message}")
        raise
```

### 5. Usage Example

```python
# Load configuration
config_loader = ConfigLoader(
    config_dir='./config',
    environment='prod',
    enable_vault=True
)

config = config_loader.load()

# Access configuration
app_name = config['application']['name']
batch_size = config_loader.get('batch', 'default_batch_size', default=5000)
db2_config = config_loader.get_db2_config()

# Create connection from config
from repository.connection_factory import ConnectionConfig

db_config = ConnectionConfig(
    database=db2_config['connection']['database'],
    hostname=db2_config['connection']['hostname'],
    port=db2_config['connection']['port'],
    uid=db2_config['connection']['uid'],
    pwd=db2_config['connection']['pwd'],
    security=db2_config['connection'].get('security')
)

# Create connection pool
pool = ConnectionPool(
    factory=ConnectionFactory(db_config),
    min_size=db2_config['connection_pool']['min_size'],
    max_size=db2_config['connection_pool']['max_size']
)
```

## Environment Variable Overrides

```bash
# Override batch size at runtime
export BATCH_BATCH_DEFAULT_BATCH_SIZE=15000

# Override connection pool size
export BATCH_DB2_CONNECTION_POOL_MAX_SIZE=25

# Override log level
export BATCH_APPLICATION_LOG_LEVEL=DEBUG

# Run batch job
python -m src.batch.app --config config/ --env prod
```

## Vault Integration

### Setup HashiCorp Vault
```bash
# Install vault client
pip install hvac

# Authenticate
export VAULT_ADDR=https://vault.company.com
export VAULT_TOKEN=s.abc123xyz789

# Store secrets
vault kv put secret/batch/prod \
  db2_uid=prod_user \
  db2_pwd=SecurePassword123!

# Verify
vault kv get secret/batch/prod
```

### CyberArk Integration (Alternative)
```python
def _load_from_cyberark(self) -> Dict[str, Any]:
    """Load secrets from CyberArk."""
    try:
        from cyberark import CyberArkClient
        
        client = CyberArkClient(
            url=self._config['cyberark']['url'],
            app_id=self._config['cyberark']['app_id']
        )
        
        password = client.get_password(
            safe=self._config['cyberark']['safe'],
            object_name=self._config['cyberark']['object_name']
        )
        
        return {'db2': {'connection': {'pwd': password}}}
        
    except Exception as e:
        logger.error(f"CyberArk integration failed: {e}")
        raise
```

## Common Pitfalls

❌ **Secrets in source control**
```yaml
# Bad - committed to git
db2:
  uid: "prod_user"
  pwd: "hardcoded_password"  # ❌ Security risk!
```

✅ **Secrets from vault or gitignored file**
```yaml
# Good - use vault reference
vault:
  enabled: true
  secret_path: "secret/data/batch/prod"
```

❌ **No environment separation**
```python
# Bad - single config for all environments
DATABASE = "PROD_DB"  # ❌ Can't test safely
```

✅ **Environment-specific configs**
```python
# Good - load by environment
config = ConfigLoader(environment='dev').load()
```

❌ **Hard-coded values in code**
```python
# Bad
BATCH_SIZE = 5000  # ❌ Requires code change to adjust
```

✅ **Configuration-driven**
```python
# Good
batch_size = config.get('batch', 'default_batch_size')
```

## Pipeline Mode Selection Guide

| Factor | Single Reader Mode | Multi-Reader Mode |
|--------|-------------------|-------------------|
| **Storage Type** | Network (NFS/SAN/cloud) | Local SSD |
| **Record Type** | Variable-length OK | Fixed-length required |
| **File Preparation** | None needed | Must pre-split files |
| **Complexity** | Simple | More complex |
| **Recommended For** | Most use cases | Maximum parallelism |

```yaml
# Single reader (default, recommended)
pipeline:
  reading_mode: "single"

# Multi-reader (requires pre-split files)
pipeline:
  reading_mode: "multi"
  multi_reader:
    input_pattern: "/data/chunks/chunk_*.dat"  # Pre-existing chunks
    # OR
    split_before_processing: true              # Split first
    split_output_dir: "/tmp/chunks"
    chunk_size_gb: 25
```

## Pipeline Tuning Guidelines

Use these guidelines to tune the producer-consumer pipeline for optimal performance:

| Parameter | Starting Value | Adjust Based On |
|-----------|----------------|-----------------|
| `num_processors` | 4 | 1 per CPU core (single-reader mode) |
| `num_writers` | 2 | Increase if queue fills up; decrease if DB overloaded |
| `processed_queue_max_size` | 10 | Increase if processors wait; decrease if memory high |
| `chunk_size` | 65536 (64KB) | Increase for fast storage; decrease for slow networks |
| `batch_size` | 5000 | Increase for throughput; decrease if DB locks occur |

### Memory Budget Calculation

```
Single Reader Mode:
  Total ≈ (num_processors × batch_buffer) + (queue_sizes × batch_size × record_size)
  
Example:
  4 processors × 25MB batch buffer = 100MB
  30 queue batches × 5000 records × 200 bytes = 30MB
  Total ≈ 130MB (well under 500MB budget)
```

### Common Tuning Scenarios

**Scenario: I/O-bound (slow storage) - Single Reader**
```yaml
pipeline:
  reading_mode: "single"
  single_reader:
    chunk_size: 131072        # 128KB chunks
  num_processors: 8           # More processors to overlap I/O waits
  num_writers: 2              # Writers aren't the bottleneck
```

**Scenario: CPU-bound (complex validation)**
```yaml
pipeline:
  reading_mode: "single"
  num_processors: 4           # Match CPU cores
  num_writers: 2
  batch_size: 2000            # Smaller batches for responsiveness
```

**Scenario: Maximum throughput (local SSD, pre-split files)**
```yaml
pipeline:
  reading_mode: "multi"
  multi_reader:
    input_pattern: "/data/chunks/chunk_*.dat"
  num_writers: 4              # More writers to parallelize inserts
  processed_queue_max_size: 20  # Larger queue
  batch_size: 10000           # Larger batches for insert efficiency
```

## Testing

```python
import pytest
from pathlib import Path
import tempfile

@pytest.fixture
def temp_config_dir():
    with tempfile.TemporaryDirectory() as tmpdir:
        config_dir = Path(tmpdir)
        
        # Create base config
        base_config = config_dir / "base_config.yaml"
        base_config.write_text("""
application:
  name: "TestApp"
db2:
  connection:
    database: "TEST_DB"
    hostname: "localhost"
    port: 50000
""")
        
        yield config_dir

def test_config_loader(temp_config_dir):
    loader = ConfigLoader(str(temp_config_dir), environment='dev')
    config = loader.load()
    
    assert config['application']['name'] == 'TestApp'
    assert config['db2']['connection']['database'] == 'TEST_DB'

def test_env_var_override(temp_config_dir, monkeypatch):
    monkeypatch.setenv('BATCH_DB2_CONNECTION_PORT', '60000')
    
    loader = ConfigLoader(str(temp_config_dir))
    config = loader.load()
    
    assert config['db2']['connection']['port'] == 60000
```

## Next Steps

- **Security**: See `07-security-compliance.md` for encryption patterns
- **Logging**: See `05-logging-monitoring.md` for structured logging
- **Deployment**: Configure per environment in CI/CD pipeline



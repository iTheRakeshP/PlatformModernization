# Section-Specific Copilot Instructions

Modular, task-focused guidance for mainframe COBOL to Python batch modernization. Each file provides complete, copy-paste ready templates for specific implementation tasks.

## Quick Navigation

| File | Size | Use When |
|------|------|----------|
| [00-core-architecture.md](00-core-architecture.md) | ~4KB | Starting new project, understanding overall design |
| [01-ebcdic-copybook-handling.md](01-ebcdic-copybook-handling.md) | ~6KB | Reading mainframe files, parsing COBOL layouts |
| [02-repository-pattern.md](02-repository-pattern.md) | ~5KB | Implementing DB2 data access layer |
| [03-configuration-management.md](03-configuration-management.md) | ~4KB | Setting up multi-environment configuration |
| [04-reconciliation.md](04-reconciliation.md) | ~5KB | Implementing data validation and audit trails |
| [05-logging-monitoring.md](05-logging-monitoring.md) | ~4KB | Adding structured logging and metrics |
| [06-testing-patterns.md](06-testing-patterns.md) | ~6KB | Writing unit and integration tests |
| [07-security-compliance.md](07-security-compliance.md) | ~5KB | Implementing PII masking, encryption, audit |
| [08-performance-tuning.md](08-performance-tuning.md) | ~4KB | Optimizing throughput and resource usage |

## How to Use

### Option 1: Load Specific Section
When working on a specific task, reference the relevant file in your Copilot chat:
```
@workspace I'm implementing EBCDIC file reading. Show me the pattern from 01-ebcdic-copybook-handling.md
```

### Option 2: Progressive Learning
Start with `00-core-architecture.md` for overall context, then dive into specific sections as needed during implementation.

### Option 3: Project Kickoff
Review all files during project planning to understand the complete solution architecture and identify reusable patterns.

## File Structure

Each section file includes:
- **Concept Overview** - Brief explanation (2-3 sentences)
- **When to Use** - Specific scenarios and triggers
- **Complete Code Template** - Production-ready, copy-paste implementation
- **Configuration** - Required settings and parameters
- **Common Pitfalls** - What to avoid and why
- **Quick Reference** - Key decisions and parameters at a glance

## Related Resources

- **Main Instructions**: `../.github/copilot-instructions.md` - Quick reference with all patterns
- **Prompts**: `../.github/MODERNIZATION_PROMPTS.md` - Ready-to-use prompts for modernization

## Typical Workflow

```
1. Project Setup
   └─> 00-core-architecture.md (understand layers)
   └─> 03-configuration-management.md (setup configs)

2. File Processing
   └─> 01-ebcdic-copybook-handling.md (read mainframe files)

3. Data Access
   └─> 02-repository-pattern.md (DB2 integration)

4. Quality Assurance
   └─> 04-reconciliation.md (data validation)
   └─> 06-testing-patterns.md (automated testing)

5. Production Readiness
   └─> 05-logging-monitoring.md (observability)
   └─> 07-security-compliance.md (security hardening)
   └─> 08-performance-tuning.md (optimization)
```

## Token Efficiency

Each file is designed to fit within Copilot's context window:
- Individual files: 3-6KB (~1,000-2,000 tokens)
- Can load 2-3 files simultaneously for complex tasks
- Total collection: ~43KB (~14,000 tokens)

---

*Last Updated: November 30, 2025*

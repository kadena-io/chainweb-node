# CI/CD Workflows

Chainweb Node uses GitHub Actions for continuous integration and deployment.

## Main Workflows

### Build and Publish Application Binaries

The main workflow for building and publishing application binaries is defined in `.github/workflows/applications.yml`.

This workflow:

1. Builds the Chainweb Node application for different GHC versions
2. Runs tests to ensure functionality
3. Creates artifacts for deployment
4. Publishes releases when triggered

### Configuration Options

The workflow can be configured with several options:

- **debugInfo**: Whether to create binaries with debugging info (DWARF debugging symbols)
- **optimizationLevel**: Optimization level used to compile Pact and Chainweb Node
- **profiling**: Enable profiling runtime
- **debug**: Compile and link with -debug
- **eventlog**: Link with -eventlog

### Build Matrix

The workflow uses a build matrix to test against multiple configurations:

- GHC versions: 9.8.2, 9.10.1, 9.6.6
- Cabal version: 3.12
- Operating system: Ubuntu 22.04

### Artifacts

The workflow produces several artifacts:

- Chainweb Node binary
- Supporting tools and utilities
- Documentation
- Configuration files

## Other Workflows

### macOS Build

Builds the application for macOS platforms.

### Nix Build

Builds the application using the Nix package manager.

### Release Workflow

Creates official releases with proper versioning and release notes.

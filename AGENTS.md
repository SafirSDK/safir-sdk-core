# AGENTS.md

This file provides guidance to AI agents when working with code in this repository.

## Project Overview

Safir SDK Core is a middleware and platform for creating distributed soft real-time systems. It provides scalable, reliable, and portable data distribution for real-time and information systems, developed over 25+ years at Saab. The SDK supports multi-language development (C++, C#, Java).

Key characteristics:
- No infinite queues, event-driven (no polling), asynchronous (no RPC/blocking calls)
- Designed for bounded latency
- Implements a distributed object cache with shared memory for local access
- UDP/IP with reliable protocol for inter-node communication

## Build Commands

### Linux (Debian/Ubuntu)
```bash
# Install prerequisites (Ubuntu 24.04/Debian 12+)
sudo apt-get install python3 pipx python-is-python3 python3-distro build-essential \
     g++ cmake default-jdk asciidoctor cli-common-dev libboost-all-dev unixodbc-dev doxygen \
     graphviz qt6-base-dev qt6-websockets-dev qt6-base-private-dev qt6-svg-dev dia dblatex \
     devscripts debhelper fakeroot ninja-build python3-websocket texlive-font-utils

# Install Conan 2
pipx install "conan>=2.5.0"

# Export required Conan recipe (from source root)
conan export build/qt-advanced-docking-system

# Build packages (creates .deb files in tmp/)
build/build.py --package

# Jenkins-style build
build/build.py --jenkins --package

# Manual CMake build (for other Linux distributions)
cmake . -DCMAKE_BUILD_TYPE=Release
make
make install
```

### Running Tests
```bash
# Run all tests via CTest (after building)
ctest

# Skip slow tests (recommended for quick iterations)
SAFIR_SKIP_SLOW_TESTS=1 ctest
```

Slow tests skipped by `SAFIR_SKIP_SLOW_TESTS`: LowLevelLogger, Communication tests, ElectionHandler tests, DOPE backend tests, restart/light node tests, WebSocket tests, system picture tests, Incarnation_And_Control_Tests, sate_script.

### CI/CD (Jenkins)

The Jenkinsfile defines a matrix build across:
- **Platforms**: ubuntu-noble, debian-trixie, vs2022, vs2026
- **Architectures**: amd64 (x86 dropped for most platforms)
- **Build types**: RelWithDebInfo, DebugOnly

Test stages:
1. Build and Unit Test
2. Standalone Tests
3. Multinode Tests
4. Multicomputer Tests (cpp only, requires debian-trixie)
5. Build Examples

## Architecture

### The Dob (Distributed Objects)

The Dob provides three distribution mechanisms:

1. **Messages** - Fire-and-forget, no guaranteed delivery, no storage
2. **Services** - Request/response with guaranteed delivery and timeout handling
3. **Entities** - Owned objects stored in Dob, with subscriptions and guaranteed delivery

### Layered Component Structure

Components are suffixed `.ss` (Safir Subsystem):

1. **LLUF (Low Level Utilities Framework)** - Foundation layer
   - `lluf_config.ss` - Configuration management
   - `lluf_utils.ss` - Utility functions
   - `lluf_crash_reporter.ss` - Crash reporting

2. **DOTS (Distributed Object Type System)** - Type system layer
   - `dots_kernel.ss` - Core type system kernel
   - `dots_cpp.ss`, `dots_dotnet.ss`, `dots_java.ss` - Language bindings
   - `dots_dobmake.ss` - Code generation tool for DOU files

3. **DOSE (Distributed Object Service Engine)** - Main middleware
   - `dose_main.ss` - Core DOSE service (started by safir_control)
   - `dose_cpp.ss`, `dose_dotnet.ss`, `dose_java.ss` - Language bindings

4. **Supporting Components**
   - `dope/` - Object persistence engine (dope_main)
   - `swre/` - Software reports/telemetry
   - `web/` - REST and WebSocket interface (safir_web)

### Key Directories
- `include/Safir/` - Public C++ headers (Dob, Logging, Application, Utilities)
- `src/` - Source code organized by component
- `src/tests/` - Integration tests
- `examples/` - Example applications (vehicleapp, vehiclemmi)
- `docs/users_guide/` - User's Guide (AsciiDoc format)

## DOU Files (Type System)

Data Object Units (DOU/DOM files) define the type system in XML:
- Inherit from `Safir.Dob.Entity`, `Safir.Dob.Message`, `Safir.Dob.Service`, etc.
- Members have IsNull and IsChanged flags
- Support for Items (complex types), Structs, Arrays, Sequences, Dictionaries
- Parameters are runtime constants read at startup

The `dobmake` tool generates language-specific code from DOU files.

## Configuration

### INI Files
Three main configuration files searched in order:
1. `/etc/safir-sdk-core/` (Linux system-wide)
2. `~/.config/safir-sdk-core/` (Linux user)

- **locations.ini** - Lock files, crash dumps, IPC endpoints
- **logging.ini** - Syslog settings, native logging
- **typesystem.ini** - Shared memory size, DOU search paths, library modules

### SAFIR_INSTANCE
Environment variable to run multiple Dob nodes on one computer. All processes with the same SAFIR_INSTANCE value form one node.

## Running the System

```bash
# Start the Dob (required before any application can connect)
safir_control

# Check system status
safir_status

# GUI for node control
safir_control_gui

# CLI for stop commands
safir_control_cli --help
```

## Debugging Tools

- **Sate** (Safir Application Tester) - GUI for interactive Dob operations, entity subscriptions, message sending. Supports scripting via JSON files.
- **Dobexplorer** - Shows memory usage graphs, node statuses, connection statistics
- **safir_tool_launcher** - Launches debug tools with specific SAFIR_INSTANCE
- **dots_configuration_check** - Validates DOU files, queries typesystem
- **Tracer** - Debug logging with `Safir.Application.Tracer` class
  - Control via `bd` command, Safir Tracer Viewer GUI, or `FORCE_LOG` env var
  - Outputs: stdout, Safir Logging, Tracer UDP protocol

## Code Style

### Python
- Formatter: YAPF with PEP8 base style, 120 character line limit
- Linter: PyLint (score threshold: 10.0)
- Naming: snake_case for functions/variables, PascalCase for classes

### C++
- Uses CMake build system
- Optional Clang-Tidy static analysis (RUN_CLANG_TIDY flag)
- Public headers in `include/Safir/`

## Multi-Language Support

The SDK provides bindings for:
- **C++** (native/primary)
- **C#/.NET** (modules suffixed `_dotnet`)
- **Java** (modules suffixed `_java`)

### Interface Design Philosophy

**IMPORTANT**: Public interfaces are available in C++, C#, and Java. When working on these interfaces, you must ensure they remain as similar as possible across all three languages, while preserving the conventions and idioms of each language. Interfaces should feel native to each language without introducing surprises.

When modifying or creating public interfaces, ensure:
- Following language-specific naming conventions (e.g., PascalCase methods in C#, camelCase in Java, snake_case or PascalCase in C++ as appropriate)
- Using language-native patterns (e.g., properties in C#, getters/setters in Java)
- Maintaining consistent behavior and semantics across all language bindings

Note for Java: Use `putVal()`/`putObj()` instead of `put()` when adding to collections.

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LispKit is a framework for building Lisp-based extension and scripting languages for macOS and iOS applications. It implements a core language based on the R7RS (small) Scheme standard, fully written in Swift 6. The framework consists of a compiler, virtual machine, garbage collector, and extensive library system.

## Build Commands

### Using Swift Package Manager

**Build debug binary:**
```bash
swift build -Xswiftc "-D" -Xswiftc "SPM"
```

**Build release binary:**
```bash
swift build -c release -Xswiftc "-D" -Xswiftc "SPM"
```

**Run REPL (debug):**
```bash
.build/debug/LispKitRepl -r Sources/LispKit/Resources -d LispKit
```

**Run REPL (release):**
```bash
.build/release/LispKitRepl -r Sources/LispKit/Resources -d LispKit
```

**Execute a Scheme program:**
```bash
.build/debug/LispKitRepl -r Sources/LispKit/Resources -d LispKit path/to/program.scm
```

### Using Makefile

**Build and run debug REPL:**
```bash
make run
```

**Build and run release REPL:**
```bash
make repl
```

**Execute a specific program:**
```bash
make run program=path/to/program.scm
```

**Run tests:**
```bash
make test
# or
swift test -Xswiftc "-D" -Xswiftc "SPM"
```

**Clean build artifacts:**
```bash
make clean
```

### Using Xcode

Open `LispKit.xcodeproj` and build using the appropriate scheme:
- **LispKit**: Core framework for macOS
- **LispKit iOS**: Core framework for iOS
- **LispKitTools**: REPL framework for macOS
- **LispKitRepl**: Command-line tool for macOS
- **LispKitRepl iOS**: iOS application with chat-style UI

## Architecture

### Core Components

**Context** (`Sources/LispKit/Runtime/Context.swift`)
- Central hub connecting all interpreter components
- Manages shared resources: heap, symbols, libraries, file handler
- Owns the evaluator, virtual machine, and global environment
- Use `LispKitContext` for standard setup or extend `Context` for customizations

**Compiler** (`Sources/LispKit/Compiler/Compiler.swift`)
- Translates Scheme expressions into bytecode
- Two-phase compilation with optional optimization
- Manages environments, captures, and constant pools
- Produces `Code` objects containing bytecode and metadata

**VirtualMachine** (`Sources/LispKit/Runtime/VirtualMachine.swift`)
- Stack-based bytecode interpreter
- Handles tail calls, continuations (`call/cc`), and `dynamic-wind`
- Frame-based stack layout with procedure, arguments, locals, and stack values
- Includes garbage collector triggering logic

**LibraryManager** (`Sources/LispKit/Runtime/LibraryManager.swift`)
- Manages loading and registration of both native (Swift) and Scheme libraries
- Lazy loading: libraries are loaded on first `lookup`
- Libraries are stored in `Sources/LispKit/Resources/Libraries/` directory

**Expr** (`Sources/LispKit/Data/`)
- Core data representation for all LispKit values
- Implemented as Swift enum with cases for all Scheme types
- Located in `Sources/LispKit/Data/` - see `Exprs.swift`, `Cell.swift`, `Symbol.swift`, etc.

### Directory Structure

**Documentation/**
- Contains documentation in Markdown format for all LispKit libraries
- The libraries are typically implemented in Sources/LispKit/Primitives

**Tests/**
- Contains the code for all unit tests

**Tests/LispKitTests/**
- Contains the code for all unit tests of the LispKit core framework
- All code is written in Swift

**Tests/LispKitTests/Code/**
- Contains parts of the unit tests that are written in Scheme
- This code is testing built-in libraries

**Sources/**
- Contains all source code for LispKit

**Sources/LispKit**
- Contains all source code for the core framework

**Sources/LispKit iOS/**
- Contains an iOS-specific configuration files

**Sources/LispKitTools/**
- Contains a macOS-specific framework for building read-eval-print (REPL) loops with a terminal-based command-line interface
- Command-line argument parsing for the REPL

**Sources/LispKitRepl/**
- Contains a macOS-specific implementation of a read-eval-print (REPL) command-line interface

**Sources/LispKitRepl iOS/**
- Contains an iOS-specific implementation of a read-eval-print (REPL) interface for iPhones
- iOS application with chat-style UI
- This is typically used in a simulator

**Sources/LispKit/Base/**
- Utilities: object pools, locks, timers, string builders
- Foundation for managing memory and threading

**Sources/LispKit/Compiler/**
- Scanner, parser, compiler, error types
- Environment (`Env`) and binding management

**Sources/LispKit/Data/**
- Core data structures: `Expr`, `Symbol`, `Cell`, `HashTable`, `Port`, `Procedure`, etc.
- Type system and equality/hashing implementations

**Sources/LispKit/Runtime/**
- Virtual machine, context, library manager, evaluator
- Garbage collection (`Heap.swift`)
- Code execution infrastructure

**Sources/LispKit/IO/**
- Port system: text and binary input/output
- Abstractions for file, string, and custom I/O sources

**Sources/LispKit/Graphics/**
- Drawing, shapes, colors, transformations
- Platform-specific implementations (separate iOS variants)

**Sources/LispKit/Primitives/**
- Native library implementations (`*Library.swift` files)
- Each file implements one or more Scheme libraries in Swift
- Subclass `NativeLibrary` and override `declarations()`, `dependencies()`, `reexports()`, `initializations()`

**Sources/LispKit/Resources/**
- Bundled Scheme libraries (`.sld` files) in `Libraries/`
- Example programs in `Examples/`
- Asset files and documentation

## Key Source Files by Subsystem

### Data Layer (Sources/LispKit/Data/)

**Expr.swift** - Core expression type
- Defines the `Expr` enum - the fundamental data type for all LispKit values
- Cases for all Scheme types: `.symbol`, `.pair`, `.string`, `.fixnum`, `.flonum`, `.vector`, `.procedure`, `.error`, etc.
- Each case wraps the underlying Swift type (e.g., `Symbol`, `Cell`, `NativeObject`)

**Type.swift** - Type enumeration
- Defines `Type` enum representing all LispKit types
- Used for type checking and reflection
- Maps one-to-one with `Expr` cases

**Symbol.swift** - Symbol implementation
- Interned string-based identifiers
- Backed by `SymbolTable` for fast equality comparison
- Support for gensyms (generated symbols) for hygienic macros

**Cell.swift** - Cons cell implementation
- Immutable list structure (car/cdr pairs)
- Managed by `ManagedObjectPool` for cyclic structure cleanup
- Separate mutable cons-cells available for Racket-style programming

**Collection.swift** - Vector and record types
- `ImmutableVector` - Fixed-size immutable vectors
- `GrowableVector` - Dynamic mutable vectors with capacity management
- `RecordType` - Record type definitions with field names
- `Record` - Record instances with typed fields

**Procedure.swift** - Procedure representation
- `Procedure` class for both native (Swift) and compiled (bytecode) procedures
- Captures parameter specifications, documentation, and implementation
- Native procedures wrap Swift closures, bytecode procedures wrap `Code` objects

**Environment.swift** - Runtime environment
- `Environment` class manages variable bindings at runtime
- Supports lexical scoping with parent chain
- Used by VM for variable lookup and mutation

**HashTable.swift** - Hash table implementation
- Mutable hash tables with custom equality and hash functions
- Support for `eq?`, `eqv?`, and `equal?` based tables
- Weak key support for caching use cases

**Equality.swift** & **Hash.swift** - Equality and hashing
- Implement `eq?` (identity), `eqv?` (value), `equal?` (structural) equality
- Custom hash functions for all expression types
- Critical for hash table and set operations

**Serialization.swift** - Expression serialization
- Serialize expressions to binary format
- Deserialize from binary with symbol table reconstruction
- Used for compiled code storage and interprocess communication

**Port.swift** - Port abstraction
- `TextInputPort`, `TextOutputPort` for character I/O
- `BinaryInputPort`, `BinaryOutputPort` for byte I/O
- Wraps sources/targets from IO layer

**CharSet.swift** - Character set implementation
- Efficient set operations on Unicode characters
- Used by `char-set?` library procedures

**NativeObject.swift** - Custom object base class
- Subclass to create custom Scheme object types
- Provides lifecycle management and type information
- Examples: drawings, images, HTTP servers, database connections

### Compiler Layer (Sources/LispKit/Compiler/)

**Scanner.swift** - Lexical analysis
- Tokenizes Scheme source code
- Handles numbers, strings, symbols, comments
- Source position tracking for error messages

**Parser.swift** - Syntax analysis
- Parses tokens into expression trees (`Expr`)
- Handles reader macros and datum labels (e.g., `#0=(1 . #0#)`)
- Integrates with `Scanner` for error reporting

**Compiler.swift** - Bytecode compilation
- Compiles expression trees to bytecode (`Code` objects)
- Two-phase compilation: analysis then code generation
- Manages lexical environments (`Env`), bindings, captures
- Optimization passes for tail calls and constant folding

**Env.swift** - Lexical environment representation
- Three environment types: `Expired`, `Global`, `Local`
- Tracks variable bindings during compilation
- Supports nested scopes with parent chain
- Distinguishes between compile-time and runtime environments

**BindingGroup.swift** - Binding management
- Groups related bindings (e.g., `letrec` variables)
- Handles forward references and mutual recursion
- Manages initialization order

**CaptureGroup.swift** - Capture management
- Tracks variables captured by closures
- Determines which variables need heap allocation (boxes)
- Critical for correct closure semantics

**Checkpointer.swift** - Compiler state checkpointing
- Saves/restores compiler state for backtracking
- Used in macro expansion and speculative compilation

**SourcePosition.swift** - Source location tracking
- Tracks file, line, column for expressions
- Enables precise error messages and debugging info

**Error Types:**
- **LexicalError.swift** - Lexical analysis errors (invalid tokens)
- **SyntaxError.swift** - Parsing errors (malformed expressions)
- **EvalError.swift** - Compilation/evaluation errors (undefined variables, type errors)
- **RuntimeError.swift** - Universal error representation with error descriptors, irritants, stack traces

### Runtime Layer (Sources/LispKit/Runtime/)

**VirtualMachine.swift** - Bytecode interpreter
- Stack-based execution engine
- Executes `Instruction` bytecode from `Code` objects
- Frame layout: [procedure, arg1, arg2, ..., argN, local1, local2, ..., localN, stack values...]
- Supports tail call optimization (TCO)
- Implements `call/cc` (continuations) and `dynamic-wind`
- Triggers garbage collection when heap fills

**Code.swift** - Executable bytecode representation
- Contains instruction array (`[Instruction]`)
- Constant pool for literals referenced by bytecode
- Code fragments for lambda expressions
- Metadata: parameter specs, captured variables, source positions

**Instruction.swift** - Bytecode instruction set
- Defines all VM instructions (load, store, call, return, branch, etc.)
- Instructions operate on VM stack and registers
- Examples: `pushConstant`, `pushLocal`, `callFunc`, `return`, `branchIf`

**Registers.swift** - VM register state
- `rid` - Unique continuation ID
- `code` - Currently executing Code object
- `captured` - Captured variables (for closures)
- `ip` - Instruction pointer
- `fp` - Frame pointer
- `initialFp` - Initial frame pointer for current call

**VirtualMachineState.swift** - Per-continuation state
- Represents a single continuation's execution state
- Contains stack, stack pointer, registers, and dynamic-wind winders
- Captured by `call/cc` to implement continuations

**Heap.swift** - Memory allocator
- Allocates and stores all `Expr` values
- Tracks allocations for garbage collection
- Provides object statistics and memory profiling

**GarbageCollector.swift** - Mark-and-sweep GC
- Marks reachable expressions from roots (stack, globals, registers)
- Sweeps unmarked objects to reclaim memory
- Triggered automatically by VM or manually by `(collect-garbage)`

**SymbolTable.swift** - Symbol interning
- Maintains global table of unique symbols
- Fast symbol equality via pointer comparison
- Supports gensym generation for hygienic macros

**Evaluator.swift** - High-level evaluation API
- Public interface for executing Scheme code
- Methods: `parse()`, `compile()`, `execute()`, `getOutput()`
- Manages read-eval-print cycle
- Used by REPL and embedding applications

**LibraryManager.swift** - Library loading system
- Lazy loading: libraries loaded on first import
- Searches for `.sld` files in resource directories
- Handles library dependencies and initialization
- Maintains loaded library cache

**Library.swift** - Library representation
- `Library` class represents a loaded library
- Tracks exports, imports, initialization state
- Both native (Swift) and Scheme libraries

**NativeLibrary.swift** - Native library base class
- Abstract base for Swift-implemented libraries
- Override methods:
  - `declarations()` - Define procedures, syntax, constants
  - `dependencies()` - Specify required libraries
  - `reexports()` - Re-export from other libraries
  - `initializations()` - Initialization expressions
- See "Creating Native Libraries" section for usage

**FileHandler.swift** - File loading
- Loads `.scm` source files and `.sld` library files
- Searches multiple directory paths
- Handles asset loading from bundles

**SourceManager.swift** - Source tracking
- Maps source positions to file URLs
- Maintains source code for error reporting
- Supports both file-based and string-based sources

**Formatter.swift** - Expression formatting
- Pretty-printing for expressions
- Configurable output styles (compact, expanded)
- Handles circular structures gracefully

**SyntaxRules.swift** - Hygienic macro system
- Implements `syntax-rules` pattern matching
- Template expansion with hygiene (gensym-based)
- Supports ellipsis patterns (`...`) for repetition

**ImportSet.swift** - Import specifications
- Represents `import` clauses with renaming/prefixing
- Handles `only`, `except`, `prefix`, `rename` modifiers
- Used by library system to resolve imports

**Features.swift** - Feature detection
- Defines supported `cond-expand` features
- Examples: `lispkit`, `r7rs`, `ratios`, `complex`, `darwin`, `ios`
- Used for conditional compilation in libraries

**ContextDelegate.swift** - Context customization protocol
- Callback protocol for customizing VM behavior
- Methods for console input/output, completion, error handling
- Implemented by REPL to provide interactive features

**Threads/** - Threading infrastructure
- **ThreadManager.swift** - Manages LispKit execution threads
- **EvalThread.swift** - Thread wrapper for executing Scheme code
- **EvalMutex.swift** - Mutex implementation for Scheme
- **EvalCondition.swift** - Condition variable implementation
- Used by `(lispkit thread)` library

### IO Layer (Sources/LispKit/IO/)

**TextInput.swift** - Text input abstraction
- `TextInput` class wraps character input sources
- Buffering and peeking support
- Line/column tracking for error reporting

**TextInputSource.swift** - Text input source protocol
- Protocol for character sources (files, strings, streams)
- `UTF8EncodedSource` - Decode UTF-8 bytes to characters
- Implement to add custom input sources

**TextOutput.swift** - Text output abstraction
- `TextOutput` class wraps character output targets
- Buffering and flushing support
- Configurable line endings

**TextOutputTarget.swift** - Text output target protocol
- Protocol for character targets (files, strings, streams)
- Implement to add custom output targets

**BinaryInput.swift** - Binary input abstraction
- `BinaryInput` class wraps byte input sources
- Lookahead and positioning support

**BinaryInputSource.swift** - Binary input source protocol
- Protocol for byte sources (files, data, streams)
- `HTTPInputStream` - HTTP response body input
- Implement to add custom binary input sources

**BinaryOutput.swift** - Binary output abstraction
- `BinaryOutput` class wraps byte output targets
- Buffering and flushing support

### Base Layer (Sources/LispKit/Base/)

**ManagedObjectPool.swift** - Managed object lifecycle
- Manages objects with potential cyclic references (Cell, Vector, Tuple)
- Weak reference tracking with periodic cleanup
- Calls `clean()` on objects during GC to break cycles
- Critical for preventing memory leaks in complex data structures

**ObjectPool.swift** - Weak object pool
- Generic weak reference pool for arbitrary objects
- Used for caching reusable objects
- Automatic cleanup of deallocated objects

**TrackedObject.swift** - Lifecycle tracking protocol
- Base class for objects that need lifecycle callbacks
- Examples: `Heap`, `VirtualMachine`, `LibraryManager`
- Supports `release()` for cleanup

**Reference.swift** - Identity-based reference type
- Base class for objects with identity-based equality
- Hashable by object identity (pointer)
- Used for symbols, ports, procedures, etc.

**Owners.swift** - Dependency tracking
- Tracks ownership relationships between objects
- Ensures objects aren't deallocated while owned
- Used for managing port lifecycles

**Bitset.swift** - Efficient integer set
- Set of non-negative integers with compact representation
- Fast operations: insert, remove, contains, union, intersection
- Used for character sets and sparse integer collections

**Boxes.swift** - Generic box types
- `ImmutableBox<T>` - Immutable value wrapper
- `MutableBox<T>` - Mutable reference wrapper
- Used to box values for capturing in closures

**MultiMap.swift** - Multi-value map
- Maps keys to multiple values
- Efficient for one-to-many relationships
- Used in compiler and library system

**Locks.swift** - Threading primitives
- `Lock` - Basic mutual exclusion lock
- `ReadWriteLock` - Reader-writer lock
- `RecursiveLock` - Reentrant lock
- Used throughout runtime for thread safety

**StringBuilder.swift** - String building
- Efficient string construction with minimal allocations
- Used by formatter and string manipulation

**ScanBuffer.swift** - Scanning utilities
- Buffered input for scanners
- Character lookahead and backtracking
- Used by lexical scanner

**Global.swift** - Global utilities and constants
- Global configuration values
- Utility functions shared across modules
- Platform-specific adaptations

### Graphics Layer (Sources/LispKit/Graphics/)

**Drawing.swift / Drawing_iOS.swift** - Drawing implementation
- `Drawing` class represents vector graphics
- Coordinate system, shapes, text, images
- Platform-specific implementations for macOS/iOS
- Exported as native object to Scheme

**DrawingDocument.swift** - Multi-page documents
- `DrawingDocument` holds multiple `Drawing` pages
- Used for PDF export with multiple pages
- Page size and metadata management

**Shape.swift** - Shape primitives
- Geometric shapes: rectangles, circles, lines, paths
- Stroke and fill attributes
- Transformable and composable

**Color.swift** - Color representation
- Color spaces: RGB, HSB, grayscale
- Alpha channel support
- Platform-specific color conversion

**Transformation.swift / Transformation_iOS.swift** - Geometric transformations
- Affine transformations: translate, scale, rotate
- Matrix operations
- Platform-specific implementations

**Barcode.swift** - Barcode generation
- QR codes and other barcode formats
- Uses Core Image filters
- Customizable size and error correction

### Primitives Layer (Sources/LispKit/Primitives/)

**LibraryRegistry.swift** - Library registration
- Central registry of all native libraries
- Static registration via `@autoclosure`
- Used by `Context` initialization to load libraries

**Key Library Implementations:**

**BaseLibrary.swift** - Core base functionality
- Special forms: `quote`, `if`, `lambda`, `define`, `set!`
- Used internally by compiler and VM
- Implements library `(lispkit base)`

**CoreLibrary.swift** - R7RS core library
- Implements `(scheme base)` from R7RS standard
- Core procedures: `cons`, `car`, `cdr`, `list`, `apply`, `map`, etc.
- Foundation for all other libraries
- Implements library `(lispkit core)`

**ControlFlowLibrary.swift** - Control flow
- `cond`, `case`, `and`, `or`, `when`, `unless`, `do`, `let` variants
- Exception handling: `guard`, `error`, `raise`, `with-exception-handler`
- Implements library `(lispkit control)`

**MathLibrary.swift** - Mathematics
- Numeric operations: arithmetic, comparison, trigonometry
- Support for exact/inexact, rational, complex numbers
- Provides implementation of math operations for numeric tower
- Implements library `(lispkit math)`

**ListLibrary.swift** - List operations
- List manipulation: `append`, `reverse`, `filter`, `fold`, etc.
- Association lists: `assoc`, `assv`, `assq`
- Implements library `(lispkit list)`

**VectorLibrary.swift** - Vector operations
- Vector creation, access, mutation
- Implements library `(lispkit vector)`

**StringLibrary.swift** - String operations
- String manipulation, searching, case conversion
- Unicode support
- Implements library `(lispkit string)`

**PortLibrary.swift** - Port and I/O operations
- File I/O, string ports, bytevector ports
- Reading and writing with various formats
- Implements `(scheme file)`, `(scheme read)`, `(scheme write)` from R7RS
- Implements library `(lispkit port)`

**SystemLibrary.swift** - System interaction
- Command-line arguments, environment variables
- File system operations (directory listing, file info)
- Process execution and exit
- Implements library `(lispkit system)`

**ThreadLibrary.swift** - Threading support
- Thread creation and management
- Mutexes and condition variables
- Implements library `(lispkit thread)`

**HTTPLibrary.swift** - HTTP client
- HTTP requests: GET, POST, PUT, DELETE
- Header and cookie management
- Response handling
- Implements library `(lispkit http)`

**HTTPServerLibrary.swift** - HTTP server
- Embedded HTTP server
- Request routing and handling
- WebSocket support
- Implements library `(lispkit http server)`

**SQLiteLibrary.swift** - SQLite database
- Database connection and query execution
- Prepared statements
- Transaction support
- Implements library `(lispkit sqlite)`

**DrawingLibrary.swift / DrawingLibrary_iOS.swift** - Drawing operations
- Create and manipulate drawings
- Render to images or PDFs
- Platform-specific implementations
- Implements library `(lispkit draw)`

**ImageLibrary.swift** - Image processing
- Image loading, saving, manipulation
- Filters and transformations
- Uses Core Image framework
- Implements library `(lispkit image)`

**CryptoLibrary.swift** - Cryptographic operations
- Hashing: MD5, SHA1, SHA256
- Encryption/decryption: AES
- Random number generation
- Implements library `(lispkit crypto)`

**JSONLibrary.swift** - JSON operations
- Parse JSON strings to Scheme data
- Serialize Scheme data to JSON
- JSONPath queries
- Implements library `(lispkit json)`

**Platform-specific Notes:**
- Libraries with `_iOS.swift` suffix are iOS-specific implementations
- **SystemCallLibrary.swift** - macOS only (not available on iOS)
- **PasteboardLibrary.swift / PasteboardLibrary_iOS.swift** - Platform-specific clipboard access

## Additional Key Patterns

### Bytecode Execution Flow

1. **Source → Tokens** - `Scanner` tokenizes source text
2. **Tokens → Expressions** - `Parser` builds `Expr` trees
3. **Expressions → Bytecode** - `Compiler` generates `Code` with instructions
4. **Bytecode → Execution** - `VirtualMachine` interprets `Instruction` stream
5. **Garbage Collection** - `GarbageCollector` reclaims unused memory periodically

### Expression Lifecycle

1. **Allocation** - Expressions created via `Heap` allocation
2. **Usage** - Passed by value (Swift enum), underlying objects referenced
3. **Marking** - GC marks reachable expressions during collection
4. **Cleanup** - `ManagedObjectPool` cleans cyclic structures
5. **Deallocation** - Unmarked objects swept by GC

### Library Loading Sequence

1. **Registration** - `LibraryRegistry` registers native libraries at startup
2. **Import** - `import` statement triggers library lookup
3. **Loading** - `LibraryManager` loads library (native or `.sld` file)
4. **Dependencies** - Recursively load dependent libraries
5. **Initialization** - Execute library initialization expressions
6. **Caching** - Store loaded library for subsequent imports

### Error Propagation

1. **Detection** - Error detected in native code or VM
2. **RuntimeError Creation** - Create `RuntimeError` with descriptor and irritants
3. **Propagation** - Throw error or return `.error(RuntimeError)` Expr
4. **Catching** - Error handlers installed via `with-exception-handler` or `guard`
5. **Reporting** - Stack trace and error message formatted for user

### Threading Model

1. **Main Context** - Primary `Context` owns heap, symbols, libraries
2. **Thread Creation** - `(thread-start!)` creates `EvalThread` with code
3. **Execution** - Thread runs in isolated VM state with shared heap
4. **Synchronization** - `EvalMutex` and `EvalCondition` coordinate threads
5. **Termination** - Thread completes or is terminated, resources cleaned up

## File Organization Conventions

**Naming Patterns:**
- `*Library.swift` - Native library implementation
- `*_iOS.swift` - iOS-specific variant of macOS file
- `*Error.swift` - Error type definition
- Test files mirror source structure in `Tests/LispKitTests/`

**Code Organization:**
- One major class/enum per file (e.g., `Compiler.swift` defines `Compiler` class)
- Related types grouped (e.g., `Collection.swift` has `Vector`, `Record`, `RecordType`)
- Platform differences handled via separate files or `#if` conditionals

**Documentation:**
- XML doc comments for public APIs
- Implementation notes in code comments
- Library documentation in `Documentation/` (Markdown files)

## Debugging Tips

**Common Investigation Paths:**

- **Compilation issues** → Check `Compiler.swift`, `Env.swift`, error types
- **Runtime errors** → Check `VirtualMachine.swift`, `Instruction.swift`, `RuntimeError.swift`
- **Memory leaks** → Check `ManagedObjectPool.swift`, `GarbageCollector.swift`, `Heap.swift`
- **Library loading problems** → Check `LibraryManager.swift`, `FileHandler.swift`, `Library.swift`
- **Type system questions** → Check `Expr.swift`, `Type.swift`, `Equality.swift`
- **Performance bottlenecks** → Check `VirtualMachine.swift`, `GarbageCollector.swift`, profiling
- **Threading issues** → Check `ThreadManager.swift`, `EvalThread.swift`, `EvalMutex.swift`

**Useful Debugging Procedures:**
- `(debug-stack)` - Print VM stack trace
- `(collect-garbage)` - Force GC and see statistics
- `(environment-bindings)` - Inspect environment
- `(procedure-code)` - Examine compiled bytecode
- `(disassemble)` - Disassemble bytecode for procedure

## Creating Native Libraries

To add new native functionality:

1. Create a new Swift file in `Sources/LispKit/Primitives/` named `<Feature>Library.swift`
2. Subclass `NativeLibrary`:
   ```swift
   public final class MyFeatureLibrary: NativeLibrary {
     public override class var name: [String] {
       return ["lispkit", "my-feature"]
     }

     public override func declarations() {
       // Define procedures, special forms, constants
       self.define(Procedure("my-proc", self.myProc))
     }

     private func myProc(_ args: Arguments) throws -> Expr {
       // Implementation
     }
   }
   ```
3. Register in `Context.swift` initialization
4. Optionally create corresponding Scheme library in `Sources/LispKit/Resources/Libraries/`

## Testing

**Test Infrastructure:**
- Tests use XCTest framework
- Base class: `LispKitTestCase` in `Tests/LispKitTests/LispKitTestCase.swift`
- Each test gets a fresh LispKit context via `setUp()`

**Data-Driven Tests:**
- Scheme test files in `Tests/LispKitTests/Code/*.scm`
- Format: `("description" expected-value source-expression)`
- Load with `loadTests(from: "filename")` and execute with `execute(tests: ...)`
- Use `<error>` symbol as expected value to test for errors

**Running Single Test:**
```bash
swift test --filter <TestClassName>.<testMethodName>
```

## Platform Differences

**macOS vs iOS:**
- iOS lacks `(lispkit system call)` library
- iOS doesn't support color lists in `(lispkit draw)`
- Some graphics files have `_iOS.swift` variants for platform-specific implementations
- Excluded iOS files listed in `Package.swift` under `exclude`

## Key Patterns

**Error Handling:**
- Throw `RuntimeError` for runtime errors in native libraries
- Use `.error(RuntimeError)` Expr case for error values

**Memory Management:**
- Objects tracked via `ManagedObjectPool` for cyclic dependency cleanup
- Expressions allocated on `Heap` with periodic garbage collection
- Use `TrackedObject` protocol for objects needing lifecycle tracking

**Immutability:**
- Lists are immutable (contrary to R7RS)
- Mutable cons-cells supported separately (Racket-style)
- Most data structures use copy-on-write or immutable designs

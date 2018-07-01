# rascal2webassembly
A WebAssembly API written in Rascal MPL.

## Setup

To be able to run the API, it should be setup within the Eclipse IDE (<http://www.eclipse.org>). These instructions describe how to do this.

### Setup test suite
Pull the test suite from the repo at <https://github.com/WebAssembly/testsuite>. The WebAssembly files in this repo seem to be using a different syntax for the memory grow and size instructions, than the syntax specification is. For now, change all occurrences of `grow_memory` to `memory.grow`, and all occurrences of `current_memory` to `memory.size`. This test suite should be setup as an Eclipse project with the name `testsuite` to be able to run the test files.

### Setup pico project
Pico is a minimal educational programming language (<http://pico.vub.ac.be>). This API includes a compiler from the Pico language into WebAssembly (in `lang::pico2wasm::Pico2Wasm`). Additionally, a project including several minimal Pico programs is included in the directory `pico`. This directory should be setup as an Eclipse project under the name `pico`, to be able to run the Pico compiler.

### Setup Rascal
See <https://www.rascal-mpl.org/start/>

## Running
Several main files are included that evaluate different aspects of the implemented API. Look at these references when building similar applications.
* *MainParse* - Evaluates parsing of the modules in the test suite
* *MainDesugar* - Evaluates desugaring the parsed modules
* *MainExecution* - Parses, desugars, converts to ADT and then runs script files in the test suite
* *MainConvert* - Converts the modules from concrete to abstract syntax, and then back and again; to evaluate the abstract to concrete conversion.
* *MainCompilePico* - Compiles the `pico/factorial.pico` program into WebAssembly and runs it.
* *EclipseSupport* - Provides some very basic eclipse integration for WebAssembly and the Pico compiler.

## WebAssembly
See <https://github.com/webassembly>

Specification: <https://webassembly.github.io/spec/core/>.\
Test suite for evaluation: <https://github.com/WebAssembly/testsuite>

## Rascal
See <https://github.com/usethesource/rascal>

## Features
Implemented features are prefixed with a :heavy_check_mark:.
* :heavy_check_mark: Concrete Syntax Grammar
* :heavy_check_mark: Abstract Syntax ADT
* :heavy_check_mark: Transpiler from Pico to WASM
* :x: - WebAssembly validator - Abandoned

## References

### WebAssembly
* Haas, A., Rossberg, A., Schuff, D., Titzer, B. (2017). *Bringing the Web up to Speed with WebAssembly*\
  (Note: Contains a reduced abstract syntax description)
* Rossberg., A. (2016). *WebAssembly: high speed at low cost for everyone*

### Rascal
* Klint, P., van der Storm, T., Vinju, J. (2009). *Rascal: a Domain Specific Language for Source Code Analysis and Manipulation*
* Klint P., van der Storm, T., Vinju. J. (2010). *EASY Meta-Programming with Rascal. Leveraging the Extract-Analyze-SYnthesize Paradigm for Meta-Programming*

### Related
* Klint, P., Lammel, R., Verhoef, C. *Toward an Engineering Discipline for Grammarware*
* Prinz, A. *Multi-level Language Descriptions*\
  Describes the levels on which a programming language can be modeled.
* Clark, T., Sammut, P., Willans, J. (2008). *Applied Metamodelling: A Foundation for Language Driven Development*\
  (Chapters 2, 4, 5)

### Unrelated
These are papers related to a limited extent.
* Donovan, A., Muth, R., Chen, B. Sehr, D. (2000). *PNaCl: Portable Native Client Executables*\
  Described a preceding technology by Google. Deprecated in favor of WebAssembly.
* Zakai, A. (2013). *Emscripten: An LLVM-to-JavaScript Compiler*\
  Describes the process of transpiling and running existing binary and low-level code in a browser, before the introduction of WebAssembly.

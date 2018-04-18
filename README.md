# rascal2webassembly
A WebAssembly API written in Rascal MPL.

## WebAssembly
See <https://github.com/webassembly>

Specification in PDF format: <https://webassembly.github.io/spec/core/_download/WebAssembly.pdf>.\
Test suite for evaluation: <https://github.com/WebAssembly/testsuite>

## Rascal
See <https://github.com/usethesource/rascal>

## Features
Implemented features are prefixed with a :heavy_check_mark:.
* Concrete Syntax Grammar
* Abstract Syntax ADT
* WebAssembly validator
* Transpiler from Pico to WASM

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
These are papers related to a limited extend.
* Donovan, A., Muth, R., Chen, B. Sehr, D. (2000). *PNaCl: Portable Native Client Executables*\
  Described a preceding technology by Google. Deprecated in favor of WebAssembly.
* Zakai, A. (2013). *Emscripten: An LLVM-to-JavaScript Compiler*\
  Describes the process of transpiling and running existing binary and low-level code in a browser, before the introduction of WebAssembly.
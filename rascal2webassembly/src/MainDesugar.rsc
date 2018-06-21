module MainDesugar

import ParseTree;
import Set;
import util::FileSystem;
import IO;
import Exception;

import lang::webassembly::script::ScriptSyntax;
import lang::webassembly::Syntax;
import lang::webassembly::Abstract;
import lang::webassembly::Desugar;
import HelpersWasm;

void main( ) {
  loc lTestSuite = |project://testsuite/|;
  set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" };
  set[loc] lFailed = { };
  
  for ( f <- lTestFiles ) {
    println( f );
    
    try {
      scriptTree = parse( #start[WebAssemblyScript], f );
    
      visit ( scriptTree ) {
      case Module m: {
        start[WebAssembly] wasmTree = (start[WebAssembly])`<Module m>`;
        desWasmTree = desugar( wasmTree );
      }
      }
    } catch RuntimeException e: {
      println( "Failed: <e>" );
      lFailed += f;
    }
    println( );
  }
  
  println( "Done. <size(lTestFiles)-size(lFailed)>/<size(lTestFiles)> succesful" );
}
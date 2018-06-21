module MainCompile

import lang::pico2wasm::Pico2Wasm;
import demo::lang::Pico::Syntax;
import demo::lang::Pico::Abstract;
import lang::webassembly::Abstract;
import HelpersWasm;
import IO;

import lang::webassembly::execution::RuntimeStructures;
import lang::webassembly::execution::RuntimeOperations;
import lang::webassembly::execution::Reduction;

import ParseTree;


// This main will compile the add.pico program into WebAssembly
// The resulting WebAssembly program will be run, outputting
//   the intermediate stack and instruction state after
//   performing a reduction step.

void main( ) {
  tree = parse( #start[Program], |project://pico/concat.pico| );
  PROGRAM picoAdt = implode( #PROGRAM, tree );
  
  MODULE modBase = pico2wasm( picoAdt );
  moduleinst modInst = setupModuleInstance( modBase );
  store s = setupStore( modBase, modInst );
    
  int funcIdx = findExportFuncIdx( modBase, "main" );
  config c = setupExecutionConfig( modBase, modInst, s, funcIdx, [ ] );
  
  println( "Stack: <stack2str( c.t.stack )>" );
  println( "Instructions: <c.t.instructions>" );
  println( );
  
  while ( !isDone( c ) ) {
    config c2 = reduce( hostFunc, c );
    
    if ( c == c2 ) {
      break;
    }
    
    c = c2;
    
    println( "Stack: <stack2str( c.t.stack )>" );
    println( "Instructions: <instrs2str(c.t.instructions)>" );
    println( );
  }
}

// not used for Pico programs.
list[runtime_val] hostFunc( str moduleName, str funcName, list[runtime_val] params ) {
  return [];
}

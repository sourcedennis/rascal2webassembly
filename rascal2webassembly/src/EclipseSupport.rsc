module EclipseSupport

import ParseTree;
import util::IDE;
import IO;
import Message;

import lang::webassembly::Syntax;
import lang::webassembly::AST2Syntax;
import lang::webassembly::script::ScriptSyntax;
import lang::webassembly::script::ScriptExecution;
import lang::webassembly::script::ScriptAbstract;
import lang::webassembly::script::ScriptSyntax2AST;
import HelpersWasm;
import HelpersPico;
import lang::pico2wasm::Pico2Wasm;

import demo::lang::Pico::Plugin;
import demo::lang::Pico::Syntax;
import demo::lang::Pico::Abstract;

private str WASM_LANGUAGE = "webassembly";
private str WASM_EXTENSION = "wat";

private str WASM_SCRIPT_LANGUAGE = "webassembly-script";
private str WASM_SCRIPT_EXTENSION = "wast";

public void registerWebAssembly( ) {
  registerLanguage( WASM_SCRIPT_LANGUAGE, WASM_SCRIPT_EXTENSION, parseWasmScript );
  registerLanguage( WASM_LANGUAGE, WASM_EXTENSION, parseWasm );
  
  registerContributions( WASM_SCRIPT_LANGUAGE, {
    popup( menu( "WebAssembly",
      [ action( "Run", void ( start[WebAssemblyScript] t, loc s ) { runScript( t, s ); } ) ] ) )
    }
  );
  
  registerPico();
  registerContributions( "Pico", {
    popup( menu( "WebAssembly",
      [ action( "Convert", void ( Program t, loc s ) { compilePico( t, s ); } ) ] ) )
    }
  );
}

private void runScript( start[WebAssemblyScript] t, loc s ) {
  try {
    println( "Running" );
    start[WebAssemblyScript] desT = desugar( t );
    WASM_SCRIPT adt = toAST( desT );
    <numAssertions, failedAssertions> = runScript( adt );
      
    println( "<numAssertions-size(failedAssertions)>/<numAssertions> Successful assertions" );
  
    if ( size( failedAssertions ) > 0 ) {
      println( "Failed assertions: " );
      for ( c <- failedAssertions ) {
        println( c );
      }
    }
  } catch e: {
    println( "Error" );
    println( e );
  }
}

private void compilePico( Program t, loc l ) {
  PROGRAM adt = toADT( t );
  MODULE modBase = pico2wasm( adt );
  start[WebAssembly] concrete = toConcrete( modBase );
  
  loc watLoc = |<l.scheme>://<l.authority><l.path>.wat|;
  
  writeFile( watLoc, "<concrete>" );
}
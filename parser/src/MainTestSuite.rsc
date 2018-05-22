module MainTestSuite

import lang::webassembly::Syntax;
import lang::webassembly::Desugar;
import lang::webassembly::ScriptConvertADT;
import lang::webassembly::ScriptSyntax;
import lang::webassembly::script::ScriptExecution;

import Helpers;
import IO;
import util::FileSystem;
import ParseTree;

void main( ) {
  loc lTestSuite = |project://testsuite/|;
  set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" }
                          - |project://testsuite/call.wast| // this one takes 10 minutes
                          - |project://testsuite/call_indirect.wast| // also slow
                          - |project://testsuite/imports.wast| // throws an exception because the host function behaves strange
                          - |project://testsuite/memory_trap.wast| // Uses up all memory.
                          - |project://testsuite/linking.wast|;
  
  int totalNumAssertions  = 0;
  int totalNumAssertionsFailed = 0;
  for ( f <- lTestFiles ) {
    //f = |project://testsuite/data.wast|;
    println( f );
    
    try {
      scriptTree = desugar( parse( #start[WebAssemblyScript], f ) );
      <numAssertions, failedAssertions> = runScript( toADT( scriptTree ) );
      
      totalNumAssertions += numAssertions;
      totalNumAssertionsFailed += size( failedAssertions );
      
      println( "<numAssertions-size(failedAssertions)>/<numAssertions> Successful assertions" );
    } catch e:StackOverflow( ): {
      println( "StackOverflow" );
    }  catch ParseError(loc l): {
      println( "Parsing failed" );
    }
  }
  
  println( "Done" );
  println( "<totalNumAssertions-totalNumAssertionsFailed>/<totalNumAssertions>" );
}

start[WebAssemblyScript] desugar( start[WebAssemblyScript] s )
  = visit( s ) {
  case Module m => desM when (start[WebAssembly])`<Module desM>` := desugar( (start[WebAssembly])`<Module m>` )
  };
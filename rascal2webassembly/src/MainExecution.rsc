module MainExecution

import lang::webassembly::Syntax;
import lang::webassembly::Desugar;
import lang::webassembly::script::ScriptSyntax2AST;
import lang::webassembly::script::ScriptSyntax;
import lang::webassembly::script::ScriptExecution;

import HelpersWasm;
import IO;
import util::FileSystem;
import ParseTree;

void main( ) {
  loc lTestSuite = |project://testsuite/|;
  set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" }
                          //- |project://testsuite/call.wast| // this one takes 10 minutes
                          //- |project://testsuite/call_indirect.wast| // also slow
                          - |project://testsuite/imports.wast| // throws an exception because the host function behaves strange
                          - |project://testsuite/linking.wast|; // linking is out-of-scope anyway
  
  int totalNumAssertions  = 0;
  int totalNumAssertionsFailed = 0;
  for ( f <- lTestFiles ) {
    //f = |project://testsuite/f64.wast|;
    println( f );
    
    try {
      scriptTree = desugar( parse( #start[WebAssemblyScript], f ) );
      ast = toAST( scriptTree );
      <numAssertions, failedAssertions> = runScript( ast );
      
      totalNumAssertions += numAssertions;
      totalNumAssertionsFailed += size( failedAssertions );
      
      if ( size( failedAssertions ) > 0 ) {
        println( "<numAssertions-size(failedAssertions)>/<numAssertions> Successful assertions" );
        /*for ( c <- failedAssertions ) {
          println( c );
        }*/
      }
    //} catch e:StackOverflow( ): {
      //println( "StackOverflow" );
    }  catch ParseError(loc l): {
      println( "Parsing failed" );
    }
  }
  
  println( "Done" );
  println( "<totalNumAssertions-totalNumAssertionsFailed>/<totalNumAssertions> Total successful assertions" );
}
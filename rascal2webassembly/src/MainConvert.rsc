module MainConvert

import ParseTree;
import IO;

import lang::webassembly::Abstract;
import lang::webassembly::Syntax2AST;
import lang::webassembly::AST2Syntax;
import lang::webassembly::Desugar;
import lang::webassembly::script::ScriptSyntax;
import HelpersWasm;

// Converts a .wat file from textual>concrete syntax>abstract syntax>concrete syntax>textual
// Note that this does not work for script files. In that case a module is extracted first

void main( ) {
  loc lTestSuite = |project://testsuite/|;
  set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" };
  
  for ( f <- lTestFiles ) {
    println( f );
    
    try {
      scriptTree = desugar( parseWasmScript( f ) );
      
      top-down-break visit ( scriptTree ) {
      // Filter out the other top-level-constructs. Only use Modules (and no non-top-level Modules)
      case Register r: { ; }
      case Action a: { ; }
      case Assertion a: { ; }
      case Module m: {
        MODULE ast1 = toAST( m );
        m2 = toConcrete( ast1 );
        MODULE ast2 = toAST( m2 );
        
        if ( ast1 == ast2 ) {
          println( "OK" );
        } else {
          println( ast1 );
          println( );
          println( m2 );
          println( );
          println( ast2 );
          break;
        }
      }
      }
    } catch StackOverflow( ): {
      ; // Stack overflows sometimes happen, but not always
        // on the same test suite. Not sure why.
      println( "Stack overflow" );
    }  catch err: {
      println( "Error" );
      println( err );
      break;
    }
  }
  
  println( "Done" );
}
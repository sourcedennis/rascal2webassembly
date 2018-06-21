module MainParse

import lang::webassembly::Syntax;
import lang::webassembly::Desugar;
import lang::webassembly::script::ScriptSyntax2AST;
import lang::webassembly::script::ScriptSyntax;
import lang::webassembly::script::ScriptExecution;
import HelpersWasm;

import IO;
import util::FileSystem;
import ParseTree;
import Map;
import String;
import Set;

void main( ) {
  loc lTestSuite = |project://testsuite/|;
  // The test suite also contains proposed extensions in sub directories
  // Therefore only take .wast files in root directory.
  set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" };
  map[loc,bool] fileParseStatus = ( f: tryParseWasmScript( f ) | f <- lTestFiles );
  
  int numSuccess = ( 0 | fileParseStatus[file] ? it + 1 : it | loc file <- fileParseStatus );
  println( "Successfully parsed: <numSuccess>/<size(fileParseStatus)>" );
  
  set[loc] unsuccessfulFiles = { l | loc l <- fileParseStatus, !fileParseStatus[l] };
  
  if ( size( unsuccessfulFiles ) > 0 ) {
    println( unsuccessfulFiles );
  }
}

bool tryParse( void( ) func ) {
  try {
    func( );
    return true;
  } catch e:ParseError(loc l): {
    println( e );
    return false;
  }
}

bool tryParseWasmScript( str s ) = tryParse( ( ) { parseWasmScript( s ); } );
bool tryParseWasmScript( loc l ) = tryParse( ( ) { parseWasmScript( l ); } );
bool tryParseWasm( str s ) = tryParse( ( ) { parseWasm( s ); } );
bool tryParseWasm( loc l ) = tryParse( ( ) { parseWasm( l ); } );
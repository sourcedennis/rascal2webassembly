module Main

import IO;
import Map;
import ParseTree;
import util::FileSystem;
import lang::webassembly::Syntax;
import lang::webassembly::ScriptSyntax;

start[WebAssembly] parseWasm( str s ) = parse( #start[WebAssembly], s );
start[WebAssemblyScript] parseWasmScript( loc l ) = parse( #start[WebAssemblyScript], l );
start[WebAssemblyScript] parseWasmScript( str s ) = parse( #start[WebAssemblyScript], s );

void main( ) {
  loc lTestSuite = |project://testsuite/|;
  // The test suite also contains proposed extensions in sub directories
  // Therefore only take .wast files in root directory.
  set[loc] lTestFiles = { f | f <- lTestSuite.ls, !isDirectory(f), f.extension == "wast" };
  map[loc,bool] fileParseStatus = ( f: tryParseWasmScript( f ) | f <- lTestFiles );
  
  int numSuccess = ( 0 | fileParseStatus[file] ? it + 1 : it | loc file <- fileParseStatus );
  println( "Successfully parsed: <numSuccess>/<size(fileParseStatus)>" );
  
  set[loc] unsuccessfulFiles = { l | loc l <- fileParseStatus, !fileParseStatus[l] };
  println( unsuccessfulFiles );
}

bool tryParse( void( ) func ) {
  try {
    func( );
    return true;
  } catch ParseError(loc l): {
    return false;
  }
}

bool tryParseWasmScript( str s ) = tryParse( ( ) { parseWasmScript( s ); } );
bool tryParseWasmScript( loc l ) = tryParse( ( ) { parseWasmScript( l ); } );
bool tryParseWasm( str s ) = tryParse( ( ) { parseWasm( s ); } );
bool tryParseWasm( loc l ) = tryParse( ( ) { parseWasm( l ); } );

test bool testParseEmpty( ) =
    tryParseWasm( "( module )" );
    
test bool testParseMemory( ) =
    tryParseWasm( "( module
                  '    ( memory 1 )
                  ')" );

test bool testParseData( ) =
    tryParseWasm( "( module
                  '    ( memory 1 )
                  '    (data (i32.const 0) \"abcdefghijklmnopqrstuvwxyz\")
                  ')" );
                  
test bool testParseExportedFunction( ) =
    tryParseWasm( "( module
                   '    (func (export \"good1\") (param $i i32) (result i32)
                   '        (i32.load8_u offset=0 (get_local $i))  ;; 97 \'a\'
                   '    )
                   ')" );

test bool testParseTwoFunctions( ) =
    tryParseWasm( "( module
                  '    ( func $name ( param $i i32 ) ( result i32 )
                  '        ( i32.load8_u offset=0 ( get_local $i ) )  ;; 97 \'a\'
                  '        ( i32.load8_u offset=0 ( get_local $i ) )  ;; 97 \'a\'
                  '    )
                  '    (func (export \"bad\") (param $i i32)
                  '        (drop (i32.load offset=4294967295 (get_local $i)))
                  '    )
                  ')" );
                  
test bool testParseBig( ) =
    tryParseWasm( "(module
                  '  (memory 1)
                  '  (data (i32.const 0) \"abcdefghijklmnopqrstuvwxyz\")
                  '  (func (export \"good1\") (param $i i32) (result i32)
                  '    (i32.load8_u offset=0 (get_local $i))  ;; 97 \'a\'
                  '  )
                  '  (func (export \"good2\") (param $i i32) (result i32)
                  '    (i32.load8_u offset=1 (get_local $i))  ;; 98 \'b\'
                  '  )
                  '  (func (export \"good3\") (param $i i32) (result i32)
                  '    (i32.load8_u offset=2 (get_local $i))  ;; 99 \'c\'
                  '  )
                  '  (func (export \"good4\") (param $i i32) (result i32)
                  '    (i32.load8_u offset=25 (get_local $i)) ;; 122 \'z\'
                  '  )
                  '  (func (export \"good5\") (param $i i32) (result i32)
                  '    (i32.load16_u offset=0 (get_local $i))          ;; 25185 \'ab\'
                  '  )
                  '  (func (export \"good6\") (param $i i32) (result i32)
                  '    (i32.load16_u align=1 (get_local $i))           ;; 25185 \'ab\'
                  '  )
                  '  (func (export \"good7\") (param $i i32) (result i32)
                  '    (i32.load16_u offset=1 align=1 (get_local $i))  ;; 25442 \'bc\'
                  '  )
                  '  (func (export \"good8\") (param $i i32) (result i32)
                  '    (i32.load16_u offset=2 (get_local $i))          ;; 25699 \'cd\'
                  '  )
                  '  (func (export \"good9\") (param $i i32) (result i32)
                  '    (i32.load16_u offset=25 align=1 (get_local $i)) ;; 122 \'z\\0\'
                  '  )
                  '  (func (export \"good10\") (param $i i32) (result i32)
                  '    (i32.load offset=0 (get_local $i))          ;; 1684234849 \'abcd\'
                  '  )
                  '  (func (export \"good11\") (param $i i32) (result i32)
                  '    (i32.load offset=1 align=1 (get_local $i))  ;; 1701077858 \'bcde\'
                  '  )
                  '  (func (export \"good12\") (param $i i32) (result i32)
                  '    (i32.load offset=2 align=2 (get_local $i))  ;; 1717920867 \'cdef\'
                  '  )
                  '  (func (export \"good13\") (param $i i32) (result i32)
                  '    (i32.load offset=25 align=1 (get_local $i)) ;; 122 \'z\\0\\0\\0\'
                  '  )
                  '  (func (export \"bad\") (param $i i32)
                  '    (drop (i32.load offset=4294967295 (get_local $i)))
                  '  )
                  ')" );

test bool testParseNotWasm( ) =
    !tryParseWasm( "print( \"Hello World!\" )" );

test bool testParseNotWasm2( ) =
    !tryParseWasm( "abcdefg" );

test bool testParseNotWasm3( ) =
    !tryParseWasm( "(module i32.load)" );

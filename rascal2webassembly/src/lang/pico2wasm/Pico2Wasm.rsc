module lang::pico2wasm::Pico2Wasm

import lang::pico2wasm::WasmFunctions;
import demo::lang::Pico::Abstract;
import lang::webassembly::Abstract;

import List;
import String;
import IO; // temp

alias byte = int;
alias bytes = list[ byte ];

int MALLOC_ID = 0;
int CONCAT_ID = 2;

data CompileContext = CompileContext( map[str,int] locals, map[str,int] constString );

list[INSTR] pico2wasm( EXP expr:id( PicoId name ), CompileContext( locals, strings ) ) = [ get_local( locals[ name ] ) ];
list[INSTR] pico2wasm( EXP expr:natCon( int iVal ), CompileContext _ ) = [ i64_const( iVal ) ];
list[INSTR] pico2wasm( EXP expr:strCon( str sVal ), CompileContext( locals, strings ) ) = [ i32_const( strings[ sVal ] ) ];
list[INSTR] pico2wasm( EXP expr:add( EXP left, EXP right ), CompileContext ctx ) = pico2wasm( left, ctx ) + pico2wasm( right, ctx ) + [ i64_add( ) ];
list[INSTR] pico2wasm( EXP expr:sub( EXP left, EXP right ), CompileContext ctx ) = pico2wasm( left, ctx ) + pico2wasm( right, ctx ) + [ i64_sub( ) ];
list[INSTR] pico2wasm( EXP expr:conc( EXP left, EXP right ), CompileContext ctx ) = pico2wasm( left, ctx ) + pico2wasm( right, ctx ) + [ call( CONCAT_ID ) ];

list[INSTR] pico2wasm( STATEMENT _:asgStat(PicoId name, EXP exp), ctx:CompileContext( locals, strings ) )
  = pico2wasm( exp, ctx ) + [ set_local( locals[ name ] ) ];

list[INSTR] pico2wasm( STATEMENT _:ifElseStat( EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart ), CompileContext ctx )
  = pico2wasm( exp, ctx ) + [ i32_wrap_i64( ), \if( resulttype( [] ), [ x | x <- pico2wasm( thenpart, ctx ) ], [ x | v <- elsepart, x <- pico2wasm( v, ctx ) ] ) ];

list[INSTR] pico2wasm( STATEMENT _:whileStat( EXP exp, list[STATEMENT] body ), CompileContext ctx )
  = [ loop( resulttype( [] ), pico2wasm( exp, ctx ) + [ i32_wrap_i64( ), ifStmt ] ) ]
  when ifStmt := \if( resulttype( [] ), [ x | v <- body, x <- pico2wasm( v, ctx ) ] + [ br( 1 ) ], [] );

MODULE pico2wasm( PROGRAM p:program(list[DECL] decls, list[STATEMENT] stats) ) {
  <ctx,memory> = setupCompileContext( p );
  
  locals = [ pico2wasm( d ) | d <- decls ];
  fType = functype( [], [] );
 
  // Reconstruct the module from the compiled function, and the native (malloc, etc.) functions 
  
  list[FUNCTYPE] fTypes;
  list[FUNC] funcs;
  EXPORT ex;
  
  if ( hasStringConcat( stats ) ) { // Import the malloc functions only if they're actually used
    f = func( size( nativeFunctionsModule.types ), locals, expr( [ x | s <- stats, x <- pico2wasm( s, ctx ) ] ) );
    fTypes = nativeFunctionsModule.types + fType;
    funcs = nativeFunctionsModule.funcs + f;
    ex = export( "main", exportdesc_func( size( nativeFunctionsModule.funcs ) ) );
  } else {
    f = func( 0, locals, expr( [ x | s <- stats, x <- pico2wasm( s, ctx ) ] ) );
    fTypes = [ fType ];
    funcs = [ f ];
    ex = export( "main", exportdesc_func( 0 ) );
  }
  
  int numPages = ( size( memory ) + 64 * 1024 - 1 ) / ( 64 * 1024 );
  
  return \module( fTypes,
                  funcs,
                  /*tables */ [],
                  /* mems */ [ mem( memtype( limits( numPages ) ) ) | numPages > 0 ],
                  /* globals */ [],
                  /* elems */ [],
                  [ \data( 0, expr( [ i32_const( 0 ) ] ), memory /* + [ 0 | i <- [0..size(memory)-numPages*64*1024] ] */ ) | numPages > 0 ],
                  /* imports */ [],
                  [ ex ] );
}

bool hasStringConcat( list[STATEMENT] stats ) {
  for ( s <- stats ) {
    if ( hasStringConcat( s ) ) {
      return true;
    }
  }
  return false;
}

bool hasStringConcat( asgStat( PicoId name, EXP exp ) )
  = hasStringConcat( exp );
bool hasStringConcat( ifElseStat( EXP exp, list[STATEMENT] thenpart, list[STATEMENT] elsepart ) )
  = hasStringConcat( exp ) || hasStringConcat( thenpart ) || hasStringConcat( elsepart );
bool hasStringConcat( whileStat( EXP exp, list[STATEMENT] body ) )
  = hasStringConcat( exp ) || hasStringConcat( body );

bool hasStringConcat( EXP _:conc( _, _ ) ) = true;
default bool hasStringConcat( EXP _ ) = false;

VALTYPE pico2wasm( DECL _:decl( PicoId name, TYPE tp ) ) = pico2wasm( tp );

VALTYPE pico2wasm( TYPE _:natural( ) ) = i64( ); // actual int
VALTYPE pico2wasm( TYPE _:string( ) ) = i32( ); // string

tuple[CompileContext,bytes] setupCompileContext( PROGRAM _:program( list[DECL] decls, list[STATEMENT] stmts ) ) {
  <strMap,memory> = getStrings( stmts );
  
  return <CompileContext( getNameIndices( decls ), strMap ), memory>;
}

map[str,int] getNameIndices( list[DECL] decls ) {
  int idx = 0;
  map[str,int] m = ( );
  
  for ( decl( PicoId name, TYPE tp ) <- decls ) {
    m[ name ] = idx;
    idx += 1;
  }
  
  return m;
}

tuple[map[str,int],bytes] getStrings( list[STATEMENT] stmts ) {
  // Make a byte array containing all strings in stmts, 0-terminated
  // The map contains pointers into the array, indexed by the literals
  
  map[str,int] strPtrs = ( );
  list[byte] strData = [ 0, 0, 0, 0 ];
  
  for ( s <- stmts ) {
    visit ( s ) {
    case strCon( str sVal ): {
      if ( !( sVal in strPtrs ) ) {
        str s = substring( sVal, 1, size( sVal ) - 1 );
        strPtrs[ sVal ] = size( strData );
        strData += chars( s ) + [ 0 ];
      } 
    }
    }
  }
  
  if ( size( strData ) == 4 ) {
    // If the only memory is the initial length identifier
    // there is no actual memory
    return <(),[]>;
  }
  
  // Store memory size in bytes as little endian in the first 4 bytes
  strData[ 3 ] = ( size( strData ) / 0x1000000 ) % 0x100;
  strData[ 2 ] = ( size( strData ) / 0x10000 ) % 0x100;
  strData[ 1 ] = ( size( strData ) / 0x100 ) % 0x100;
  strData[ 0 ] = size( strData ) % 0x100;
  
  return <strPtrs, strData>;
}
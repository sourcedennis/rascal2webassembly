module lang::pico2wasm::Pico2Wasm

import lang::pico2wasm::WasmFunctions;
import demo::lang::Pico::Abstract;
import lang::webassembly::ADT;

import List;


alias byte = int;
alias bytes = list[ byte ];

int MALLOC_ID = 0;
int CONCAT_ID = 1;

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
  f = func( size( nativeFunctionsModule.types ), locals, expr( [ x | s <- stats, x <- pico2wasm( s, ctx ) ] ) );
  fType = functype( [], [] );
 
  // Reconstruct the module from the compiled function, and the native (malloc, etc.) functions 
  
  list[FUNCTYPE] fTypes = nativeFunctionsModule.types + fType;
  list[FUNC] funcs = nativeFunctionsModule.funcs + f;
  EXPORT ex = export( "main", exportdesc_func( size( nativeFunctionsModule.funcs ) ) );
  
  int numPages = ( size( memory ) + 64 * 1024 - 1 ) / ( 64 * 1024 );
  
  return \module( fTypes,
                  funcs,
                  /*tables */ [],
                  /* mems */ [ mem( memtype( limits( numPages ) ) ) ],
                  /* globals */ [],
                  /* elems */ [],
                  [ \data( 0, expr( [ i32_const( 0 ) ] ), memory + [ 0 | i <- [0..size(memory)-numPages*64*1024] ] ) ],
                  /* imports */ [],
                  [ ex ] );
}

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
  // TODO: Make a byte array containing all strings in stmts, 0-terminated
  //       The map contains pointers into the array, indexed by the literals
  return <( ), []>; // TODO
}
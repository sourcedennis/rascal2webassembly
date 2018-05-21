module lang::webassembly::ScriptExecution

import lang::webassembly::ScriptADT;
import lang::webassembly::Execution;
import lang::webassembly::Float;
import util::Math;
import List;
import IO; // temp

// Scripts are not executed using small-step. Instead, all assert rules are executed
//   and the result will determine which ones failed. (For now)

// Requires the values to be cleaned with clean()
bool eq( CONST_f32( a ), CONST_f32( b ) ) = eq( a, b );
bool eq( CONST_f64( a ), CONST_f64( b ) ) = eq( a, b );
bool eq( CONST a, CONST b ) = ( a == b );
bool eq( list[CONST] a, list[CONST] b ) = ( size( a ) == size( b ) && ( size( a ) == 0 || all( n <- [ eq( aElem, bElem ) | <aElem,bElem> <- zip( a, b ) ], n ) ) );

void runScript( script_module( list[WASM_SCRIPT_ENTRY] entries ) ) {
  println( "Run script" );
  
  for ( entry <- entries ) {
    moduleinst modInst = setupModuleInstance( entry.\module );
    store s = setupStore( entry.\module, modInst );
    
    list[runtime_val] hostFunc( str moduleName, str funcName, list[runtime_val] params ) {
      println( "Host func" );
      return [];
    }
  
    startFuncIdx = findStartFuncIdx( entry.\module );
    if ( startFuncIdx != -1 ) {
      try {
        config c = setupExecutionConfig( entry.\module, modInst, s, startFuncIdx, [ ] );
        while ( !isDone( c ) ) {
          c = reduce( c, hostFunc );
        }
        mInst = c.t.baseFrame.\module;
        s = c.s;
      } catch ex: {
        println( ex );
      }
    }
    
    for ( line <- entry.lines ) {
      println( line );
      
      switch ( line ) {
      case assertion( assert_return( ACTION action, list[CONST] expected ) ): {
        <modInst,s2,res> = performAction( entry.\module, modInst, s, hostFunc, action );
        s = s2;
        if ( !eq( res, expected ) ) {
          println( "Invalid!" );
          println( res );
          println( expected );
          println( );
          
          return;
        }
      }
      case action( ACTION a ): {
        <modInst,s2,res> = performAction( entry.\module, modInst, s, hostFunc, a );
        s = s2;
      }
      }
    }
  }
}

tuple[moduleinst,store,list[CONST]] performAction( MODULE modBase, moduleinst modInst, store s, HostFunction hostFunc, invoke( str name, list[CONST] arguments ) ) {
  int funcIdx = findExportFuncIdx( modBase, name );
  config c = setupExecutionConfig( modBase, modInst, s, funcIdx, [ toRuntimeVal( a ) | a <- arguments ] );
  
  //println( );
  
  while ( !isDone( c ) ) {
    /*if ( name == "while" ) {
      println( stack2str( c.t.stack ) );
      println( );
    }*/
    
    c = reduce( c, hostFunc );
  }
  
  return <modInst,c.s,[ toConst( r ) | r <- getResult( c ) ]>;
}

tuple[moduleinst,store,list[CONST]] performAction( MODULE modBase, moduleinst modInst, store s, HostFunction hostFunc, get( str name ) ) {
  runtime_val val = findExportGlobalVal( modBase, modInst, name );
  println( "Get" );
  println( val );
  
  return <modInst, s, [] >;
}

str stack2str( Stack s ) = "[" + joinStr( [ stack2str( v ) | v <- s ], ", " ) + "]";
str stack2str( sev( v ) ) = "<v>";
str stack2str( sei( i ) ) = "<i>";
str stack2str( sec( c ) ) = "<c>";
str stack2str( sel( retArity, _ ) ) = "#label(<retArity>)";
str stack2str( sef( frame( locals, retArity, _ ) ) ) = "frame(<retArity>,<locals>)";
str joinStr( [], str delim ) = "";
str joinStr( [e], str delim ) = e;
str joinStr( [e, *L], str delim ) = e + delim + joinStr( L, delim );

runtime_val toRuntimeVal( CONST_i32( int ival ) ) = i32( ival );
runtime_val toRuntimeVal( CONST_i64( int ival ) ) = i64( ival );
runtime_val toRuntimeVal( CONST_f32( Float fval ) ) = f32( clean( fval, 32 ) );
runtime_val toRuntimeVal( CONST_f64( Float fval ) ) = f64( clean( fval, 64 ) );

CONST toConst( runtime_val v:i32( int ival ) ) = CONST_i32( ival );
CONST toConst( runtime_val v:i64( int ival ) ) = CONST_i64( ival );
CONST toConst( runtime_val v:f32( Float fval ) ) = CONST_f32( clean( fval, 32 ) );
CONST toConst( runtime_val v:f64( Float fval ) ) = CONST_f64( clean( fval, 64 ) );
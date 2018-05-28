module lang::webassembly::script::ScriptExecution

import util::Maybe;
import util::Math;
import List;
// import IO; // temp

import lang::webassembly::script::ScriptADT;
import lang::webassembly::execution::RuntimeStructures;
import lang::webassembly::execution::RuntimeOperations;
import lang::webassembly::execution::Reduction;
import util::Float;

// Scripts are not executed using small-step. Instead, all assert rules are executed
//   and the result will determine which ones failed. (For now)


data FailedAssertion = FailedAssertion( ASSERTION a, Maybe[list[CONST]] actual );

// Requires the values to be cleaned with clean()
bool eq( CONST_f32( a ), CONST_f32( b ) ) = eq( a, b );
bool eq( CONST_f64( a ), CONST_f64( b ) ) = eq( a, b );
bool eq( CONST a, CONST b ) = ( a == b );
bool eq( list[CONST] a, list[CONST] b ) = ( size( a ) == size( b ) && ( size( a ) == 0 || all( n <- [ eq( aElem, bElem ) | <aElem,bElem> <- zip( a, b ) ], n ) ) );

tuple[int numExecuted,list[FailedAssertion] failedAssertions] runScript( script_module( list[WASM_SCRIPT_ENTRY] entries ) ) {
  //println( "Run script" );
  int numExecuted = 0;
  list[FailedAssertion] failedAssertions = [];
  
  for ( entry <- entries ) {
    moduleinst modInst = setupModuleInstance( entry.\module );
    store s = setupStore( entry.\module, modInst );
    
    list[runtime_val] hostFunc( str moduleName, str funcName, list[runtime_val] params ) {
      //println( "Host func" );
      return [];
    }
  
    // Call the start function
    startFuncIdx = findStartFuncIdx( entry.\module );
    if ( startFuncIdx != -1 ) {
      if ( isImportedFunc( entry.\module, startFuncIdx ) ) {
        nothing( ); // empty
      } else {
        config c = setupExecutionConfig( entry.\module, modInst, s, startFuncIdx, [ ] );
        while ( !isDone( c ) ) {
          c = reduce( hostFunc, c );
        }
        s = c.s;
      }
    }
    
    // Execute the assertion lines
    for ( line <- entry.lines ) {
      //println( line );
      
      switch ( line ) {
      case assertion( a:assert_return( ACTION action, list[CONST] expected ) ): {
        <modInst,s2,mRes> = performAction( entry.\module, modInst, s, hostFunc, action );
        s = s2;
        if ( just( res ) := mRes ) {
          if ( !eq( res, expected ) ) {
            failedAssertions += FailedAssertion( a, just( res ) );
          }
        } else {
          failedAssertions += FailedAssertion( a, nothing( ) );
        }
        numExecuted = numExecuted + 1;
      }
      case assertion( a:assert_trap( ACTION action ) ): {
        <modInst,s2,mRes> = performAction( entry.\module, modInst, s, hostFunc, action );
        s = s2;
        if ( just( res ) := mRes ) {
          failedAssertions += FailedAssertion( a, just( res ) );
        }
        numExecuted = numExecuted + 1;
      }
      case assertion( a:assert_return_canonical_nan( ACTION action ) ): {
        <modInst,s2,mRes> = performAction( entry.\module, modInst, s, hostFunc, action );
        s = s2;
        if ( just( res ) := mRes ) {
          if ( !eq( res, [ CONST_f32( canonical_nan( ) ) ] ) && !eq( res, [ CONST_f64( canonical_nan( ) ) ] ) ) {
            failedAssertions += FailedAssertion( a, just( res ) );
          }
        } else {
          failedAssertions += FailedAssertion( a, nothing( ) );
        }
        numExecuted = numExecuted + 1;
      }
      case assertion( a:assert_return_arithmetic_nan( ACTION action ) ): {
        <modInst,s2,mRes> = performAction( entry.\module, modInst, s, hostFunc, action );
        s = s2;
        if ( just( res ) := mRes ) {
          if ( !eq( res, [ CONST_f32( arithmetic_nan( ) ) ] ) && !eq( res, [ CONST_f64( arithmetic_nan( ) ) ] ) ) {
            failedAssertions += FailedAssertion( a, just( res ) );
          }
        } else {
          failedAssertions += FailedAssertion( a, nothing( ) );
        }
        numExecuted = numExecuted + 1;
      }
      case action( ACTION a ): {
        <modInst,s2,res> = performAction( entry.\module, modInst, s, hostFunc, a );
        s = s2;
      }
      }
    }
  }
  
  return <numExecuted, failedAssertions>;
}

tuple[moduleinst,store,Maybe[list[CONST]]] performAction( MODULE modBase, moduleinst modInst, store s, HostFunction hostFunc, invoke( str name, list[CONST] arguments ) ) {
  int funcIdx = findExportFuncIdx( modBase, name );
  config c = setupExecutionConfig( modBase, modInst, s, funcIdx, [ toRuntimeVal( a ) | a <- arguments ] );
  
  //bool isPrintingStack = false;
  
  while ( !isDone( c ) ) {
    /*if ( isPrintingStack ) {
      println( "Stack=<stack2str( c.t.stack )>" );
      println( "Instr=<c.t.instructions>" );
      println( );
    }*/
      
    c = reduce( hostFunc, c );
  }
  /*if ( isPrintingStack ) {
    println( "Stack=<stack2str( c.t.stack )>" );
    println( "Instr=<c.t.instructions>" );
    println( );
  }*/
  
  if ( hasTrapped( c ) ) {
    return <modInst, c.s, nothing( )>;
  } else {
    return <modInst, c.s, just( [ toConst( r ) | r <- getResults( c ) ] )>;
  }
}

tuple[moduleinst,store,Maybe[list[CONST]]] performAction( MODULE modBase, moduleinst modInst, store s, HostFunction hostFunc, get( str name ) ) {
  runtime_val val = findExportGlobalVal( modInst, s, name );
  return <modInst, s, just( [ toConst( val ) ] ) >;
}

/*
private str stack2str( Stack s ) = "[" + intercalate( ", ", [ stack2str( v ) | v <- s ] ) + "]";

private str stack2str( sev( v ) ) = "<v>";
private str stack2str( sel( retArity, _ ) ) = "#label(<retArity>)";
private str stack2str( sef( retArity, frame( locals, _ ) ) ) = "frame(<retArity>,<locals>)";
*/

runtime_val toRuntimeVal( CONST_i32( int ival ) ) = i32( ival );
runtime_val toRuntimeVal( CONST_i64( int ival ) ) = i64( ival );
runtime_val toRuntimeVal( CONST_f32( Float fval ) ) = f32( fval ); // TODO: clean?. Though, sometimes introduces problems
runtime_val toRuntimeVal( CONST_f64( Float fval ) ) = f64( fval ); // TODO: clean?

CONST toConst( runtime_val v:i32( int ival ) ) = CONST_i32( ival );
CONST toConst( runtime_val v:i64( int ival ) ) = CONST_i64( ival );
CONST toConst( runtime_val v:f32( Float fval ) ) = CONST_f32( fval ); // TODO: clean?
CONST toConst( runtime_val v:f64( Float fval ) ) = CONST_f64( fval ); // TODO: clean?

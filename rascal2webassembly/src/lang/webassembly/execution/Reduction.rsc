module lang::webassembly::execution::Reduction

import List;
import Exception;
import IO; // temp

import lang::webassembly::Abstract;
import lang::webassembly::execution::RuntimeStructures;
import lang::webassembly::execution::RuntimeOperations;
import lang::webassembly::execution::ReductionConfig;
import lang::webassembly::execution::ReductionSimple;

// Public interface:
// - config reduce( HostFunction, config ): Performs a single small-step reduction

// Reduction rules are divided over three files:
// - ReductionSimple: Contains all reduction rules that only modify the stack
//                    and instruction stream
// - ReductionConfig: Contains all reduction rules of instructions with a fixed
//                    parameter count, but do update the configuration
// - Reduction: This file. Contains the most complex rules, involving instructions
//              that can have an arbitrary number of parameters.

public alias HostFunction = list[runtime_val]( str moduleName, str funcName, list[runtime_val] params );

// invoke( ): Function call
public config reduce( HostFunction hf, config( s:store( funcs, _, _, _ ), thread( S, [ sec( invoke( funcaddr addr ) ), *I ] ) ) ) {
  if ( funcinst( functype( paramTypes, resultTypes ), moduleinst m, FUNC adtFunc ) := funcs[ addr ] ) {
    // Non-host function call
    params = [ p | v <- drop( size( S ) - size( paramTypes ), S ), sev( p ) := v ];
    assert size( params ) == size( paramTypes );
    locals = [ runtimeInit( lt ) | lt <- funcs[ addr ].code.locals ];
    newFrame = frame( params + locals, getModuleInst( S ) );
    int retArity = size( resultTypes );
    blockInstr = block( resulttype( resultTypes ), adtFunc.body.instrs );
    return config( s, thread( take( size( S ) - size( paramTypes ), S ) + [ sef( retArity, newFrame ) ], [ sei( blockInstr ), sec( end( ) ), *I ] ) );
  } else if ( funcinst( functype( paramTypes, resultTypes ), str hostModule, str hostFunc ) := funcs[ addr ] ) {
    // Host function call
    params = [ p | v <- drop( size( S ) - size( paramTypes ), S ), sev( p ) := v ];
    assert size( params ) == size( paramTypes );
    try {
      results = hf( hostModule, hostFunc, params );
      assert size( results ) == size( resultTypes );
      return config( s, thread( take( size( S ) - size( paramTypes ), S ) + [ sev( v ) | v <- results ], I ) );
    } catch ex: {
      return config( s, thread( [], [ sei( trap( ) ) ] ) );
    }
  }
}

private bool isValue( sev( v ) ) = true;
private bool isValue( stackelem e ) = false;

// end( ): Naturally return from a function or label (block,if,loop)
public config reduce( HostFunction hf, config( store s, thread( S, [ sec( end( ) ), *I ] ) ) ) {
  // Find function frame or label
  list[runtime_val] values = [ v | sev( v ) <- reverse( takeWhile( reverse( S ), isValue ) ) ];
  S = take( size( S ) - size( values ), S );
  
  if ( [ *S2, sel( retArity, instrs ) ] := S ) {
    // According to the spec, no arity check of results is necessary here
    // as validation ensures that: size( values ) == statement's arity
    // (Note that a loop label's arity is always 0, even if it does return something
    //    - it's just than an explicit break to a loop is actually a "continue" statement
    //    which should not return anything)
    
    // Apparently the instructions inside the label do not repeat here, instead they fall off
    // So, for a loop to continue, an explicit break has to be inserted to the start of the loop
    return config( s, thread( S2 + [ sev( v ) | v <- values ], I ) );
  } else if ( [ *S2, sef( retArity, frame ) ] := S ) {
    assert size( values ) >= retArity;
    list[runtime_val] results = drop( size( values ) - retArity, values );
    return config( s, thread( S2 + [ sev( v ) | v <- results ], I ) );
  } else {
    throw AssertionFailed( "There must be a frame or label on the stack" );
  }
}

// return( ): Explicit return from a function
public config reduce( HostFunction hf, config( store s, thread( S, [ sei( \return( ) ), *I ] ) ) ) {
  bool isFrame( stackelem e:sef( _, _ ) ) = true;
  bool isFrame( stackelem e ) = false;
  
  bool isNotFrame( stackelem v ) = !isFrame( v );
  
  list[runtime_val] values = [ v | sev( v ) <- reverse( takeWhile( reverse( S ), isValue ) ) ];
  Stack notFrames = reverse( takeWhile( reverse( S ), isNotFrame ) );
  int numLabels = size( [ x | x <- notFrames, sel( _, _ ) := x ] );
  S = take( size(S)-size(notFrames), S );
  
  if ( [ *S2, sef( retArity, frame ) ] := S ) {
    assert size( values ) >= retArity;
    list[runtime_val] results = drop( size( values ) - retArity, values );
    // There are 'numLabels' labels on the stack, and one stack frame
    // Therefore, also pop 'numLabels+1' 'end' pseudo instructions from the instruction stream
    return config( s, thread( S2 + [ sev( v ) | v <- results ], cutEnds( numLabels + 1, I ) ) );
  } else {
    throw AssertionFailed( "There must be a frame on the stack" );
  }
}

private list[instrelem] cutEnds( int numEnds:0, list[instrelem] instructions ) = instructions;
private list[instrelem] cutEnds( int numEnds, [ sec( end( ) ), *I ] ) = cutEnds( numEnds - 1, I );
private list[instrelem] cutEnds( int numEnds, [ _, *I ] ) = cutEnds( numEnds, I );
private list[instrelem] cutEnds( int numEnds, list[instrelem] _:[] ) = Throw( AssertionFailed( "There must be enough end() instructions" ) );

// This cuts away 'numLabels' labels UP TO the 'numLabels+1' label
private Stack cutLabels( int numLabels:0, S:[*_, sel(_,_)] ) = S;
private Stack cutLabels( int numLabels, [*S, sel(_,_)] ) = cutLabels( numLabels - 1, S );
private Stack cutLabels( int numLabels, [*S, sef(_,_)] ) = \throw( AssertionFailed( "There must be enough labels. Cannot break out of a function." ) );
private Stack cutLabels( int numLabels, [*S, _] ) = cutLabels( numLabels, S );
private Stack cutLabels( int numEnds, Stack _:[] ) = Throw( AssertionFailed( "There must be enough labels" ) );

// br( ): Break to a label
public config reduce( HostFunction hf, config( store s, thread( S, [ sei( br( LABELIDX idx ) ), *I ] ) ) ) {
  list[runtime_val] values = [ v | sev( v ) <- reverse( takeWhile( reverse( S ), isValue ) ) ];
  
  if ( [ *S2, sel( retArity, instrs ) ] := cutLabels( idx, S ) ) {
    assert size( values ) >= retArity;
    list[runtime_val] results = drop( size( values ) - retArity, values );
    return config( s, thread( S2 + [ sev( v ) | v <- results ], [ sei( i ) | i <- instrs ] + cutEnds( idx + 1, I ) ) );
  } else {
    throw AssertionFailed( "There must be a label on the stack" );
  }
}

public default config reduce( HostFunction hf, config c:config( store s, thread t ) ) {
  // Apply one of the rules in 'ReductionConfig', which do modify the configuration
  if ( just( c2 ) := reduce( c ) ) {
    return c2;
  }
  // Apply one of the rules in 'ReductionSimple', which only modify the stack and instructions
  if ( just( t2 ) := reduce( t ) ) {
    return config( s, t2 );
  }
  throw AssertionFailed( "No reduction rule could be applied" );
}
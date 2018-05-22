module lang::webassembly::execution::ReductionConfig

import util::Maybe;
import List;

import util::LittleEndian;
import util::Float;
import lang::webassembly::execution::RuntimeStructures;
import lang::webassembly::execution::RuntimeOperations;
import lang::webassembly::execution::Numerics;
import lang::webassembly::ADT;

// Contains all reduction rules that need or modify the configuration

// call_indirect
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( i ) ) ], [ sei( call_indirect( TYPEIDX ti ) ), *I ] ) ) ) {
  modInst = getModuleInst( S );
  ta = modInst.tableaddrs[ 0 ];
  tableinst tab = store.tables[ ta ];
  ftExpect = modInst.types[ ti ];
  
  if ( i < size( tab.elem ) && funcelem( funcaddr a ) := tab.elem[i] ) {
    f = store.funcs[ a ];
    ftActual = f.\type;
    if ( ftActual == ftExpect ) {
      return just( config( store, thread( S, [ sec( invoke( a ) ), *I ] ) ) );
    } else {
      return just( config( store, thread( [], [ sec( trap( ) ) ] ) ) );
    }
  } else {
    return just( config( store, thread( [], [ sec( trap( ) ) ] ) ) );
  }
}

// ## variable instructions
// get_local( )
Maybe[config] reduce( config( store, thread( S, [ sei( get_local( int lIdx ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( getLocal( S, lIdx ) ) ], I ) ) );
// set_local
Maybe[config] reduce( config( store, thread( [ *S, sev( val ) ], [ sei( set_local( int lIdx ) ), *I ] ) ) )
  = just( config( store, thread( setLocal( S, lIdx, val ), I ) ) );
// tee_local: Similar to set_local, but the value remains on the stack
Maybe[config] reduce( config( store, thread( [ *S, sev( val ) ], [ sei( tee_local( int lIdx ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( val ), sev( val ) ], [ sei( set_local( lIdx ) ), *I ] ) ) );
// get_global
Maybe[config] reduce( config( store, thread( S, [ sei( get_global( int lIdx ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( getGlobal( store, lIdx ) ) ], I ) ) );
// set_global
Maybe[config] reduce( config( store, thread( [ *S, sev( val ) ], [ sei( set_global( int lIdx ) ), *I ] ) ) )
  = just( config( setGlobal( store, lIdx, val ), thread( S, I ) ) );

// ## memory instructions
// load
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i32_load( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 4 ) );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 8 ) );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( f32_load( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( f32( v ) ) ], I ) ) )
  when v := toFloat( getMemoryBytes( store, location + offset, 4 ) );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( f64_load( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( f64( v ) ) ], I ) ) )
  when v := toFloat( getMemoryBytes( store, location + offset, 8 ) );
// load8_u > i32
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i32_load8_u( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 1 ) );
// load8_s > i32
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i32_load8_s( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( v32 ) ) ], I ) ) )
  when v8 := intLE( getMemoryBytes( store, location + offset, 1 ) ),
       v32 := invSigned( 32, signed( 8, v8 ) );
// load8_u > i64
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load8_u( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 1 ) );
// load8_s > i64
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load8_s( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v64 ) ) ], I ) ) )
  when v8 := intLE( getMemoryBytes( store, location + offset, 1 ) ),
       v64 := invSigned( 64, signed( 8, v8 ) );
// load16_u > i32
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i32_load16_u( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 2 ) );
// load16_s > i32
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i32_load16_s( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( v32 ) ) ], I ) ) )
  when v16 := intLE( getMemoryBytes( store, location + offset, 2 ) ),
       v32 := invSigned( 32, signed( 16, v16 ) );
// load16_u > i64
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load16_u( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 2 ) );
// load16_s > i64
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load16_s( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v64 ) ) ], I ) ) )
  when v16 := intLE( getMemoryBytes( store, location + offset, 2 ) ),
       v64 := invSigned( 64, signed( 16, v16 ) );
// load32_u > i64
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load32_u( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v ) ) ], I ) ) )
  when v := intLE( getMemoryBytes( store, location + offset, 4 ) );
// load32_s > i64
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ) ], [ sei( i64_load32_s( offset, align ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i64( v64 ) ) ], I ) ) )
  when v32 := intLE( getMemoryBytes( store, location + offset, 4 ) ),
       v64 := invSigned( 64, signed( 32, v32 ) );
// store
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i32( val ) ) ], [ sei( i32_store( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 4, val ) ), thread( S, I ) ) );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i64( val ) ) ], [ sei( i64_store( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 8, val ) ), thread( S, I ) ) );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( f32( val ) ) ], [ sei( f32_store( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, toBytes( 4, val ) ), thread( S, I ) ) );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( f64( val ) ) ], [ sei( f64_store( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, toBytes( 8, val ) ), thread( S, I ) ) );
// i32 > store8
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i32( val ) ) ], [ sei( i32_store8( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 4, val )[0..1] ), thread( S, I ) ) );
// i32 > store16
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i32( val ) ) ], [ sei( i32_store16( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 4, val )[0..2] ), thread( S, I ) ) );
// i64 > store8
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i64( val ) ) ], [ sei( i64_store8( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 8, val )[0..1] ), thread( S, I ) ) );
// i64 > store16
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i64( val ) ) ], [ sei( i64_store16( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 8, val )[0..2] ), thread( S, I ) ) );
// i64 > store32
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( location ) ), sev( i64( val ) ) ], [ sei( i64_store32( offset, align ) ), *I ] ) ) )
  = just( config( setMemoryBytes( store, location + offset, bytesLE( 8, val )[0..4] ), thread( S, I ) ) );
// memory_size
Maybe[config] reduce( config( store, thread( S, [ sei( memory_size( ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( memorySize( store ) ) ) ], I ) ) );
// memory_grow
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( numPages ) ) ], [ sei( memory_grow( ) ), *I ] ) ) )
  = just( config( newStore, thread( [ *S, sev( i32( memorySize( store ) ) ) ], I ) ) )
  when just( newStore ) := memoryGrow( store, numPages );
Maybe[config] reduce( config( store, thread( [ *S, sev( i32( numPages ) ) ], [ sei( memory_grow( ) ), *I ] ) ) )
  = just( config( store, thread( [ *S, sev( i32( -1 ) ) ], I ) ) );

default Maybe[config] reduce( config c ) = nothing( );

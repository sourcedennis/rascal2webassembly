module lang::webassembly::Execution

import lang::webassembly::ADT;
import lang::webassembly::ExecutionFunctions;
import List;
import util::Math;
import Exception;
import IO; // temp

// Sets up an execution config to the module's start function
//   Note: Modules without a start function are not supported (for now)
config setupExecutionConfig( m:\module( types, list[FUNC] funcs, tables, mems, globals, elems, \data, START st, imports, exports ) ) {
  FUNC startFunc = funcs[ getStartFuncIdx( st ) ];
  moduleinst mInst = moduleinst( types, [0..size(funcs)], [0..size(tables)], [0..size(mems)], [0..size(globals)], [ /* TODO */ ] );
  frame f = frame( setupFuncLocals( types, startFunc ), mInst );
  store s = store( setupFuncInsts( mInst, types, funcs ), setupTableinsts( tables, elems ), setupMeminsts( mems, \data ), toGlobalinsts( globals ) ); // TODO
  thread t = thread( f, [ sei( i ) | i <- getFuncInstrs( startFunc ) ] );
  return config( s, t );
}

data TABLE = table( TABLETYPE \type );
data ELEM = elem( TABLEIDX, EXPR offset, list[FUNCIDX] init );
data TABLETYPE = tabletype( LIMITS l, ELEMTYPE e );
data LIMITS = limits( int min, int max )
            | limits( int min )
            ;
            
data funcelem = funcelem( funcaddr addr )
              | funcelem( )
              ;
              
data funcinst = funcinst( FUNCTYPE \type, moduleinst \module, FUNC code );
data tableinst = tableinst( list[funcelem] elem, int max )
               | tableinst( list[funcelem] elem )
               ;
  
// TODO: Make sure it is done once the function's result is obtained
bool isDone( config( store s, thread( frame f, [] ) ) ) = true;
default bool isDone( config c ) = false;

// values
maybe[Stack] reduce( [ sei( i32_const( ival ) ), *L ] ) = ok( [ sev( i32( ival ) ), *L ] );
maybe[Stack] reduce( [ sei( i64_const( ival ) ), *L ] ) = ok( [ sev( i64( ival ) ), *L ] );
maybe[Stack] reduce( [ sei( f32_const( fval ) ), *L ] ) = ok( [ sev( f32( fval ) ), *L ] );
maybe[Stack] reduce( [ sei( f64_const( fval ) ), *L ] ) = ok( [ sev( f64( fval ) ), *L ] );
// # iunop
// clz
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( i32_clz( ) ), *L ] ) = ok( [ sev( i32( iclz( ival, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( i64_clz( ) ), *L ] ) = ok( [ sev( i64( iclz( ival, 64 ) ) ), *L ] );
// ctz
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( i32_ctz( ) ), *L ] ) = ok( [ sev( i32( ictz( ival, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( i64_ctz( ) ), *L ] ) = ok( [ sev( i64( ictz( ival, 64 ) ) ), *L ] );
// popcnt
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( i32_popcnt( ) ), *L ] ) = ok( [ sev( i32( ipopcnt( ival ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( i64_popcnt( ) ), *L ] ) = ok( [ sev( i64( ipopcnt( ival ) ) ), *L ] );
// # funop
// abs
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_abs( ) ), *L ] ) = ok( [ sev( f32( abs( fval ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( f64_abs( ) ), *L ] ) = ok( [ sev( f64( abs( fval ) ) ), *L ] );
// neg
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_neg( ) ), *L ] ) = ok( [ sev( f32( -fval ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival ) ), sei( f64_neg( ) ), *L ] ) = ok( [ sev( f64( -fval ) ), *L ] );
// sqrt
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_sqrt( ) ), *L ] ) = ok( [ sev( f32( sqrt( fval ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival ) ), sei( f64_sqrt( ) ), *L ] ) = ok( [ sev( f64( sqrt( fval ) ) ), *L ] );
// ceil
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_ceil( ) ), *L ] ) = ok( [ sev( f32( ceil( fval ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival ) ), sei( f64_ceil( ) ), *L ] ) = ok( [ sev( f64( ceil( fval ) ) ), *L ] );
// floor
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_floor( ) ), *L ] ) = ok( [ sev( f32( floor( fval ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival ) ), sei( f64_floor( ) ), *L ] ) = ok( [ sev( f64( floor( fval ) ) ), *L ] );
// trunc
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_trunc( ) ), *L ] ) = ok( [ sev( f32( truncf( fval ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival ) ), sei( f64_trunc( ) ), *L ] ) = ok( [ sev( f64( truncf( fval ) ) ), *L ] );
// nearest
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f32_nearest( ) ), *L ] ) = ok( [ sev( f32( fnearest( fval ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival ) ), sei( f64_nearest( ) ), *L ] ) = ok( [ sev( f64( fnearest( fval ) ) ), *L ] );
// # ibinop
// add
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_add( ) ), *L ] ) = ok( [ sev( i32( ( ival1 + ival2 ) % pow2( 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_add( ) ), *L ] ) = ok( [ sev( i64( ( ival1 + ival2 ) % pow2( 64 ) ) ), *L ] );
// sub: Note that i32s are interally represented as unsigned (non-negative, that is)
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_sub( ) ), *L ] ) = ok( [ sev( i32( ( ival1 - ival2 + pow2( 32 ) ) % pow2( 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_sub( ) ), *L ] ) = ok( [ sev( i64( ( ival1 - ival2 + pow2( 64 ) ) % pow2( 64 ) ) ), *L ] );
// mul
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_mul( ) ), *L ] ) = ok( [ sev( i32( ( ival1 * ival2 ) % pow2( 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_mul( ) ), *L ] ) = ok( [ sev( i64( ( ival1 * ival2 ) % pow2( 64 ) ) ), *L ] );
// div_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_div_u( ) ), *L ] ) = ok( [ sev( i32( idiv_u( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_div_u( ) ), *L ] ) = ok( [ sev( i64( idiv_u( ival1, ival2, 64 ) ) ), *L ] );
// div_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_div_s( ) ), *L ] ) = ok( [ sev( i32( idiv_s( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_div_s( ) ), *L ] ) = ok( [ sev( i64( idiv_s( ival1, ival2, 64 ) ) ), *L ] );
// rem_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_rem_u( ) ), *L ] ) = ok( [ sev( i32( rem_u( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_rem_u( ) ), *L ] ) = ok( [ sev( i64( rem_u( ival1, ival2, 64 ) ) ), *L ] );
// rem_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_rem_s( ) ), *L ] ) = ok( [ sev( i32( irem_s( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_rem_s( ) ), *L ] ) = ok( [ sev( i64( irem_s( ival1, ival2, 64 ) ) ), *L ] );
// and
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_and( ) ), *L ] ) = ok( [ sev( i32( iand( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_and( ) ), *L ] ) = ok( [ sev( i64( iand( ival1, ival2, 64 ) ) ), *L ] );
// or
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_or( ) ), *L ] ) = ok( [ sev( i32( ior( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_or( ) ), *L ] ) = ok( [ sev( i64( ior( ival1, ival2, 64 ) ) ), *L ] );
// xor
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_xor( ) ), *L ] ) = ok( [ sev( i32( ixor( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_xor( ) ), *L ] ) = ok( [ sev( i64( ixor( ival1, ival2, 64 ) ) ), *L ] );
// shl
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_shl( ) ), *L ] ) = ok( [ sev( i32( ishl( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_shl( ) ), *L ] ) = ok( [ sev( i64( ishl( ival1, ival2, 64 ) ) ), *L ] );
// shr_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_shr_u( ) ), *L ] ) = ok( [ sev( i32( ishr_u( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_shr_u( ) ), *L ] ) = ok( [ sev( i64( ishr_u( ival1, ival2, 64 ) ) ), *L ] );
// shr_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_shr_s( ) ), *L ] ) = ok( [ sev( i32( ishr_s( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_shr_s( ) ), *L ] ) = ok( [ sev( i64( ishr_s( ival1, ival2, 64 ) ) ), *L ] );
// rotl
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_rotl( ) ), *L ] ) = ok( [ sev( i32( irotl( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_rotl( ) ), *L ] ) = ok( [ sev( i64( irotl( ival1, ival2, 64 ) ) ), *L ] );
// rotr
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_rotr( ) ), *L ] ) = ok( [ sev( i32( irotr( ival1, ival2, 32 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_rotr( ) ), *L ] ) = ok( [ sev( i64( irotr( ival1, ival2, 64 ) ) ), *L ] );
// ## fbinop
// add
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_add( ) ), *L ] ) = ok( [ sev( f32( ival1 + ival2 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_add( ) ), *L ] ) = ok( [ sev( f64( ival1 + ival2 ) ), *L ] );
// sub
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_sub( ) ), *L ] ) = ok( [ sev( f32( ival1 - ival2 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_sub( ) ), *L ] ) = ok( [ sev( f64( ival1 - ival2 ) ), *L ] );
// mul
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_mul( ) ), *L ] ) = ok( [ sev( f32( ival1 * ival2 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_mul( ) ), *L ] ) = ok( [ sev( f64( ival1 * ival2 ) ), *L ] );
// div
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_div( ) ), *L ] ) = ok( [ sev( f32( ival1 / ival2 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_div( ) ), *L ] ) = ok( [ sev( f64( ival1 / ival2 ) ), *L ] );
// min
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_min( ) ), *L ] ) = ok( [ sev( f32( min( ival1, ival2 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_min( ) ), *L ] ) = ok( [ sev( f64( min( ival1, ival2 ) ) ), *L ] );
// max
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_max( ) ), *L ] ) = ok( [ sev( f32( max( ival1, ival2 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_max( ) ), *L ] ) = ok( [ sev( f64( max( ival1, ival2 ) ) ), *L ] );
// copysign
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_copysign( ) ), *L ] ) = ok( [ sev( f32( fcopysign( ival1, ival2 ) ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_copysign( ) ), *L ] ) = ok( [ sev( f64( fcopysign( ival1, ival2 ) ) ), *L ] );
// ## itestop
// eqz
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( i32_eqz( ) ), *L ] ) = ok( [ sev( i32( ival == 0 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( i64_eqz( ) ), *L ] ) = ok( [ sev( i32( ival == 0 ? 1 : 0 ) ), *L ] );
// ## irelop
// eq
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_eq( ) ), *L ] ) = ok( [ sev( i32( ival1 == ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_eq( ) ), *L ] ) = ok( [ sev( i32( ival1 == ival2 ? 1 : 0 ) ), *L ] );
// ne
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_ne( ) ), *L ] ) = ok( [ sev( i32( ival1 != ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_ne( ) ), *L ] ) = ok( [ sev( i32( ival1 != ival2 ? 1 : 0 ) ), *L ] );
// lt_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_lt_u( ) ), *L ] ) = ok( [ sev( i32( ival1 < ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_lt_u( ) ), *L ] ) = ok( [ sev( i32( ival1 < ival2 ? 1 : 0 ) ), *L ] );
// lt_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_lt_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) < signed( ival2 ) ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_lt_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) < signed( ival2 ) ? 1 : 0 ) ), *L ] );
// gt_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_gt_u( ) ), *L ] ) = ok( [ sev( i32( ival1 > ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_gt_u( ) ), *L ] ) = ok( [ sev( i32( ival1 > ival2 ? 1 : 0 ) ), *L ] );
// gt_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_gt_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) > signed( ival2 ) ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_gt_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) > signed( ival2 ) ? 1 : 0 ) ), *L ] );
// le_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_le_u( ) ), *L ] ) = ok( [ sev( i32( ival1 <= ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_le_u( ) ), *L ] ) = ok( [ sev( i32( ival1 <= ival2 ? 1 : 0 ) ), *L ] );
// le_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_le_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) <= signed( ival2 ) ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_le_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) <= signed( ival2 ) ? 1 : 0 ) ), *L ] );
// ge_u
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_ge_u( ) ), *L ] ) = ok( [ sev( i32( ival1 >= ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_ge_u( ) ), *L ] ) = ok( [ sev( i32( ival1 >= ival2 ? 1 : 0 ) ), *L ] );
// ge_s
maybe[Stack] reduce( [ sev( i32( ival1 ) ), sev( i32( ival2 ) ), sei( i32_ge_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) >= signed( ival2 ) ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( i64( ival1 ) ), sev( i64( ival2 ) ), sei( i64_ge_s( ) ), *L ] ) = ok( [ sev( i32( signed( ival1 ) >= signed( ival2 ) ? 1 : 0 ) ), *L ] );
// ## frelop
// eq
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_eq( ) ), *L ] ) = ok( [ sev( i32( ival1 == ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_eq( ) ), *L ] ) = ok( [ sev( i32( ival1 == ival2 ? 1 : 0 ) ), *L ] );
// ne
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_ne( ) ), *L ] ) = ok( [ sev( i32( ival1 != ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_ne( ) ), *L ] ) = ok( [ sev( i32( ival1 != ival2 ? 1 : 0 ) ), *L ] );
// lt
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_lt( ) ), *L ] ) = ok( [ sev( i32( ival1 < ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_lt( ) ), *L ] ) = ok( [ sev( i32( ival1 < ival2 ? 1 : 0 ) ), *L ] );
// gt
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_gt( ) ), *L ] ) = ok( [ sev( i32( ival1 > ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_gt( ) ), *L ] ) = ok( [ sev( i32( ival1 > ival2 ? 1 : 0 ) ), *L ] );
// le
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_lt( ) ), *L ] ) = ok( [ sev( i32( ival1 <= ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_lt( ) ), *L ] ) = ok( [ sev( i32( ival1 <= ival2 ? 1 : 0 ) ), *L ] );
// ge
maybe[Stack] reduce( [ sev( f32( ival1 ) ), sev( f32( ival2 ) ), sei( f32_gt( ) ), *L ] ) = ok( [ sev( i32( ival1 >= ival2 ? 1 : 0 ) ), *L ] );
maybe[Stack] reduce( [ sev( f64( ival1 ) ), sev( f64( ival2 ) ), sei( f64_gt( ) ), *L ] ) = ok( [ sev( i32( ival1 >= ival2 ? 1 : 0 ) ), *L ] );
// ## cvtop
// i32_wrap_i64: Convert a 64-bit integer to a 32-bit integer
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( i32_wrap_i64( ) ), *L ] ) = ok( [ sev( i32( ival % pow2( 32 ) ) ), *L ] );
// i64_extend_u_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( i64_extend_u_i32( ) ), *L ] ) = ok( [ sev( i64( ival ) ), *L ] );
// i64_extend_s_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( i64_extend_s_i32( ) ), *L ] ) = ok( [ sev( i64( invSigned( signed( ival, 32 ), 64 ) ) ), *L ] );
// i32_trunc_u_f32
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( i32_trunc_u_f32( ) ), *L ] ) = ok( [ sev( i32( trunc_u( fval, 32 ) ) ), *L ] );
// i32_trunc_s_f32
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( i32_trunc_s_f32( ) ), *L ] ) = ok( [ sev( i32( trunc_s( fval, 32 ) ) ), *L ] );
// i32_trunc_u_f64
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( i32_trunc_u_f64( ) ), *L ] ) = ok( [ sev( i32( trunc_u( fval, 32 ) ) ), *L ] );
// i32_trunc_s_f64
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( i32_trunc_s_f64( ) ), *L ] ) = ok( [ sev( i32( trunc_s( fval, 32 ) ) ), *L ] );
// i64_trunc_u_f32
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( i64_trunc_u_f32( ) ), *L ] ) = ok( [ sev( i64( trunc_u( fval, 64 ) ) ), *L ] );
// i64_trunc_s_f32
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( i64_trunc_s_f32( ) ), *L ] ) = ok( [ sev( i64( trunc_s( fval, 64 ) ) ), *L ] );
// i64_trunc_u_f64
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( i64_trunc_u_f64( ) ), *L ] ) = ok( [ sev( i64( trunc_u( fval, 64 ) ) ), *L ] );
// i64_trunc_s_f64
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( i64_trunc_s_f64( ) ), *L ] ) = ok( [ sev( i64( trunc_s( fval, 64 ) ) ), *L ] );
// f32_demote_f64
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( f32_demote_f64( ) ), *L ] ) = ok( [ sev( f32( fval ) ), *L ] );
// f64_promote_f32
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( f64_promote_f32( ) ), *L ] ) = ok( [ sev( f64( fval ) ), *L ] );
// f32_convert_u_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( f32_convert_u_i32( ) ), *L ] ) = ok( [ sev( f32( toReal( ival ) ) ), *L ] );
// f32_convert_s_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( f32_convert_s_i32( ) ), *L ] ) = ok( [ sev( f32( toReal( signed( ival, 32 ) ) ) ), *L ] );
// f32_convert_u_i64
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( f32_convert_u_i64( ) ), *L ] ) = ok( [ sev( f32( toReal( ival ) ) ), *L ] );
// f32_convert_s_i64
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( f32_convert_s_i64( ) ), *L ] ) = ok( [ sev( f32( toReal( signed( ival, 64 ) ) ) ), *L ] );
// f64_convert_u_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( f64_convert_u_i32( ) ), *L ] ) = ok( [ sev( f64( toReal( ival ) ) ), *L ] );
// f64_convert_s_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( f64_convert_s_i32( ) ), *L ] ) = ok( [ sev( f64( toReal( signed( ival, 32 ) ) ) ), *L ] );
// f64_convert_u_i64
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( f64_convert_u_i64( ) ), *L ] ) = ok( [ sev( f64( toReal( ival ) ) ), *L ] );
// f64_convert_s_i64
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( f64_convert_s_i64( ) ), *L ] ) = ok( [ sev( f64( toReal( signed( ival, 64 ) ) ) ), *L ] );
// i32_reinterpret_f32
maybe[Stack] reduce( [ sev( f32( fval ) ), sei( i32_reinterpret_f32( ) ), *L ] ) = ok( [ sev( i32( invIbits( fbits( fval, 32 ) ) ) ), *L ] );
// i64_reinterpret_f64
maybe[Stack] reduce( [ sev( f64( fval ) ), sei( i64_reinterpret_f64( ) ), *L ] ) = ok( [ sev( i64( invIbits( fbits( fval, 64 ) ) ) ), *L ] );
// f32_reinterpret_i32
maybe[Stack] reduce( [ sev( i32( ival ) ), sei( f32_reinterpret_i32( ) ), *L ] ) = ok( [ sev( i32( invFbits( ibits( ival, 32 ) ) ) ), *L ] );
// f64_reinterpret_i64
maybe[Stack] reduce( [ sev( i64( ival ) ), sei( f64_reinterpret_i64( ) ), *L ] ) = ok( [ sev( i64( invFbits( ibits( ival, 64 ) ) ) ), *L ] );
// ## parametric instructions
// drop( ): Drop top value from the stack
maybe[Stack] reduce( [ sev( _ ), sei( drop( ) ), *L ] ) = ok( L );
// select( ): If c != 0 (thus, true), push first, otherwise push second - similar to conditional ( c ? val1 : val2 )
maybe[Stack] reduce( [ sev( i32( c ) ), sev( v1 ), sev( v2 ), sei( select( ) ), *L ] ) = ok( [ sev( c != 0 ? v1 : v2 ), *L ] );


// ## control instructions
// nop
maybe[Stack] reduce( [ sei( nop( ) ), *L ] ) = ok( L );
// unreachable
maybe[Stack] reduce( [ sei( unreachable( ) ), *L ] ) = ok( [ sec( trap( ) ), *L ] );
// block
maybe[Stack] reduce( [ sei( block( restype, instrs ) ), *L ] ) = ok( [ sel( arity( restype ), [] ) ] + [ sei( i ) | i <- instrs ] + sec( end( ) ) + L );
// loop
maybe[Stack] reduce( [ sei( loop( restype, instrs ) ), *L ] ) = ok( [ sel( arity( restype ), instrs ) ] + [ sei( i ) | i <- instrs ] + sec( end( ) ) + L );
// if
maybe[Stack] reduce( [ sev( i32( v ) ), sei( \if( restype, ifInstrs, elseInstrs ) ), *L ] ) {
  if ( v != 0 ) {
    return ok( sel( arity( restype ), [] ) + [ sei( i ) | i <- ifInstrs ] + sec( end( ) ) + L );
  } else {
    return ok( sel( arity( restype ), [] ) + [ sei( i ) | i <- elseInstrs ] + sec( end( ) ) + L );
  }
}
// br, br_if, br_table, return, call, and call_indirect are in the final reduce(config) case

// call( )
maybe[Stack] reduce( [ sei( call( FUNCIDX fi ) ), *L ] ) = ok( [ sec( invoke( fi ) ), *L ] );
// br_if
maybe[Stack] reduce( [ sev( i32( 0 ) ), sei( br_if( LABELIDX idx ) ), *L ] ) = ok( L );
maybe[Stack] reduce( [ sev( i32( c ) ), sei( br_if( LABELIDX idx ) ), *L ] ) = ok( [ sei( br( idx ) ), *L ] );
// br_table
maybe[Stack] reduce( [ sev( i32( i ) ), sei( br_table( labelIdxs, defaultLabelIdx ) ), *L ] ) {
  if ( i < size( labelIdxs ) ) {
    return ok( [ sei( br( labelIdxs[ i ] ) ), *L ] );
  } else {
    return ok( [ sei( br( defaultLabelIdx ) ), *L ] );
  }
}
// call_indirect
config reduce( config( s, thread( frame, [ sev( i32( i ) ), sei( call_indirect( TYPEIDX ti ) ), *L ] ) ) ) {
  ta = frame.\module.tableaddrs[ 0 ];
  tableinst tab = s.tables[ ta ];
  ftExpect = frame.\module.types[ ti ];
  
  if ( i >= size( tab.elem ) ) {
    return config( s, thread( frame, [ trap( ) ] ) );
  }
  if ( funcelem( funcaddr a ) := tab.elem[i] ) {
    f = s.funcs[ a ];
    ftActual = f.\type;
    if ( ftActual == ftExpect ) {
      return config( s, thread( frame, [ sec( invoke( a ) ), *L ] ) );
    } else {
      return config( s, thread( frame, [ trap( ) ] ) );
    }
  } else {
    return config( s, thread( frame, [ trap( ) ] ) );
  }
}

default maybe[Stack] reduce( Stack s ) = empty( );

// ## variable instructions
// get_local( )
config reduce( config( s, thread( frame, [ sei( get_local( int lIdx ) ), *L ] ) ) )
  = config( s, thread( frame, [ sev( getlocal( frame, lIdx ) ), *L ] ) );
// set_local
config reduce( config( s, thread( frame, [ sev( val ), sei( set_local( int lIdx ) ), *L ] ) ) )
  = config( s, thread( frame2, L2 ) )
  when <frame2,L2> := setlocal( frame, L, lIdx, val );
// tee_local: Similar to set_local, but the value remains on the stack
config reduce( config( s, thread( frame, [ sev( val ), sei( tee_local( int lIdx ) ), *L ] ) ) )
  = config( s, thread( frame, [ sev( val ), sev( val ), sei( set_local( lIdx ) ), *L ] ) );
// get_global
config reduce( config( s, thread( frame, [ sei( get_global( int lIdx ) ), *L ] ) ) )
  = config( s, thread( frame, [ sev( getglobal( s, lIdx ) ), *L ] ) );
// set_global
config reduce( config( s, thread( frame, [ sev( val ), sei( set_global( int lIdx ) ), *L ] ) ) )
  = config( setglobal( s, lIdx, val ), thread( frame, L ) );

// ## memory instructions
// load
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i32_load( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 4 ) ) );
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 8 ) ) );
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( f32_load( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( f32( v ) ), *L ] ) )
  when v := invFbits( toBits( getMemoryBytes( s, 0, location + offset, 4 ) ) );
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( f64_load( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( f64( v ) ), *L ] ) )
  when v := invFbits( toBits( getMemoryBytes( s, 0, location + offset, 8 ) ) );
// load8_u > i32
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i32_load8_u( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 1 ) ) );
// load8_s > i32
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i32_load8_s( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( v32 ) ), *L ] ) )
  when v8 := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 1 ) ) ),
       v32 := invSigned( signed( v8, 8 ), 32 );
// load8_u > i64
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load8_u( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 1 ) ) );
// load8_s > i64
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load8_s( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v64 ) ), *L ] ) )
  when v8 := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 1 ) ) ),
       v64 := invSigned( signed( v8, 8 ), 64 );
// load16_u > i32
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i32_load16_u( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 2 ) ) );
// load16_s > i32
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i32_load16_s( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( v32 ) ), *L ] ) )
  when v16 := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 2 ) ) ),
       v32 := invSigned( signed( v16, 16 ), 32 );
// load16_u > i64
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load16_u( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 2 ) ) );
// load16_s > i64
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load16_s( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v64 ) ), *L ] ) )
  when v16 := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 2 ) ) ),
       v64 := invSigned( signed( v16, 16 ), 64 );
// load32_u > i64
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load32_u( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v ) ), *L ] ) )
  when v := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 4 ) ) );
// load32_s > i64
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sei( i64_load32_s( offset, align ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i64( v64 ) ), *L ] ) )
  when v32 := invIbits( toBits( getMemoryBytes( s, 0, location + offset, 4 ) ) ),
       v64 := invSigned( signed( v32, 32 ), 64 );
// store
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i32( val ) ), sei( i32_store( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 32 ) ), thread( baseFrame, L ) );
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i64( val ) ), sei( i64_store( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 64 ) ), thread( baseFrame, L ) );
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( f32( val ) ), sei( f32_store( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 32 ) ), thread( baseFrame, L ) );
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( f64( val ) ), sei( f64_store( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 64 ) ), thread( baseFrame, L ) );
// i32 > store8
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i32( val ) ), sei( i32_store8( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 32 )[0..1] ), thread( baseFrame, L ) );
// i32 > store16
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i32( val ) ), sei( i32_store16( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 32 )[0..2] ), thread( baseFrame, L ) );
// i64 > store8
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i64( val ) ), sei( i64_store8( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 64 )[0..1] ), thread( baseFrame, L ) );
// i64 > store16
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i64( val ) ), sei( i64_store16( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 64 )[0..2] ), thread( baseFrame, L ) );
// i64 > store32
config reduce( config( s, thread( baseFrame, [ sev( i32( location ) ), sev( i64( val ) ), sei( i64_store32( offset, align ) ), *L ] ) ) )
  = config( setMemoryBytes( s, 0, location + offset, toBytes( val, 64 )[0..4] ), thread( baseFrame, L ) );
// memory_size
config reduce( config( s, thread( baseFrame, [ sei( memory_size( ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( memorySize( s ) ) ), *L ] ) );
// memory_grow
config reduce( config( s, thread( baseFrame, [ sev( i32( numPages ) ), sei( memory_grow( ) ), *L ] ) ) )
  = config( newS, thread( baseFrame, [ sev( i32( memorySize( s ) ) ), *L ] ) )
  when ok( newS ) := growMemory( s, numPages );
default config reduce( config( s, thread( baseFrame, [ sev( i32( numPages ) ), sei( memory_grow( ) ), *L ] ) ) )
  = config( s, thread( baseFrame, [ sev( i32( -1 ) ), *L ] ) );

frame currFrame( frame base, [] ) = base;
frame currFrame( frame base, [*L, sef(retArity,f)] ) = f;
frame currFrame( frame base, [*L, x] ) = currFrame( base, L );

// This is an auxiliary rule to accomodate for executing instructions further from the
//   top of the stack, if no valid instruction is currently on top.
//   Some of the clauses account for cases with an arbitrary number of parameters
default config reduce( c:config( s:store( funcs, tables, mems, globals ), thread( f:frame(_,modInst), Stack stack ) ) ) {
  <baseStack, valStack, instrStack> = splitAtInstrs( stack );
  
  // First try to apply one of the operators with a variable number of operators.
  if ( [ sec( invoke( funcaddr addr ) ), *L ] := instrStack
       && funcinst( functype( params, results ), m, adtFunc ) := funcs[ addr ]
       && size( params ) == size( valStack ) ) {
    // It's a function call
    // Assert: params are of correct types, by validation
    frame newFrame = frame( valStack + [ initLocal( localType ) | localType <- funcs[ addr ].code.locals ], modInst );
    stackelem blockInstr = sei( block( resulttype( results ), getFuncInstrs( adtFunc ) ) );
    return config( s, thread( f, baseStack + [ sef( size( results ), newFrame ), blockInstr ] + L ) );
  } else if ( [ *baseBaseStack, sef( retArity, frame ) ] := baseStack
              && retArity == size( valStack ) ) {
    // Return from a function
    return config( s, thread( f, baseBaseStack + [ sev( v ) | v <- valStack ] + instrStack ) );
  } else if ( [ sec( end( ) ), *L ] := instrStack
              // Note that if return values have incorrect arity, an error will be reported here !
              && <base2,l:sel(retArity, instrs)> := takeLabel( baseStack, 0 )
              && retArity == size( valStack ) ) {
    // Naturally return from a block
    return config( s, thread( f, base2 + [ sev( i ) | i <- valStack ] + cutEnd( instrStack, 0 ) ) );
  }  else if ( [ sei( br( LABELIDX idx ) ), *L ] := instrStack
               && <base2,l:sel(retArity, instrs)> := takeLabel( baseStack, idx )
               && retArity == size( valStack ) ) {
    // Jump to a label
    return config( s, thread( f, base2 + [ sev( i ) | i <- valStack ] + cutEnd( instrStack, idx ) ) );
  } else if ( ok( stack2 ) := reduce( [ sev( v ) | v <- valStack ] + instrStack ) ) {
    // Try to apply one of the reduction rules that does not affect the configuration
    return config( s, thread( currFrame( f, baseStack ), baseStack + stack2 ) );
  } else {
    // No function call. Nothing else matched. So skip a hole (e.g. a constant value) on the stack, and try again
    
    stack = [ sev( v ) | v <- valStack ] + instrStack;
    if ( size( baseStack ) == 0 ) {
      baseStack = [ head( stack ) ];
      stack = tail( stack );
    }
    
    if ( size( stack ) > 0
         && config( s2, thread( f2, L2 ) ) := reduce( config( s, thread( currFrame( f, baseStack ), stack ) ) ) ) {
      return config( s2, thread( f2, baseStack + L2 ) );
    } else {
      // this does not happen if the module is valid and everything is implemented properly
      println( "Does not happen" );
      return c;
    }
  }
  
  /*
  | br( LABELIDX li )
  | br_if( LABELIDX li )
  | br_table( list[LABELIDX] labels, LABELIDX \default )
  | \return( )
  | call( FUNCIDX fi )
  | call_indirect( TYPEIDX ti )
  */

  /*if ( [ sec( invoke( funcaddr addr ) ), *L ] := instrStack
       && funcinst( functype( params, results ), m, adtFunc ) := funcs[ addr ]
       && size( params ) == size( values ) ) {
  } else if ( [ sef( resArity, fFrame ), *L ] := instrStack
              && resArity == size( values ) ) {
    // Return from a function
    return config( s, thread( f, [ sev( i ) | i <- values ] + L ) );
  } else if ( [ sel( resArity, instrs ), *L ] := instrStack
              && resArity == size( values ) ) {
    // Pass a label
    return config( s, thread( f, [ sev( i ) | i <- values ] + [ sei( i ) | i <- instrs ] + L ) );
  } else if ( [ sei( br( LABELIDX idx ) ), *L ] := instrStack
              && <l:sel(retArity, instrs),L2> := takeLabel( L, idx )
              && retArity == size( values ) ) {
    // Jump to a label
    return config( s, thread( f, [ sev( i ) | i <- values ] + L2 ) );
  } else if ( ok( stack2 ) := reduce( stack ) ) { // Try to apply one of the reduction rules that does not affect the configuration
    return config( s, thread( f, stack2 ) );
  } else if ( [ *_, sec( trap( ) ), *_ ] := stack ) { // Shortcut for immediately "bubbling up" a trap
    return config( s, thread( f, [ sec( trap( ) ) ] ) );
  } else if ( size( values ) > 0 && config( s2, thread( f2, L2 ) ) := reduce( config( s, thread( f, [ sev( v ) | v <- tail( values ) ] + instrStack ) ) ) ) {
    // No function call. Nothing else matched. So skip a constant, and try again
    return config( s2, thread( f2, sev( head( values ) ) + L2 ) );
  } else {
    // this does not happen if the module is valid and everything is implemented properly
    return c;
  }*/
}

// Splits Stack into <Stack nonInstrs,Stack instrs>
tuple[Stack,list[runtime_val],Stack] splitAtInstrs( [*L, i:sei( _ )] ) = <a,b,c+i>
  when <a,b,c> := splitAtInstrs( L );
tuple[Stack,list[runtime_val],Stack] splitAtInstrs( [*L, i:sec( _ )] ) = <a,b,c+i>
  when <a,b,c> := splitAtInstrs( L );
tuple[Stack,list[runtime_val],Stack] splitAtInstrs( L ) = <a,b,[]>
  when <a,b> := splitAtValues( L );
tuple[Stack,list[runtime_val]] splitAtValues( [*L, sev( v )] ) = <a,b+v>
  when <a,b> := splitAtValues( L );
tuple[Stack,list[runtime_val]] splitAtValues( L ) = <L,[]>;

Stack cutEnd( [ sec( end( ) ), *L ], 0 ) = L;
Stack cutEnd( [ sec( end( ) ), *L ], int N ) = cutEnd( L, N - 1 );
Stack cutEnd( [ x, *L ], int N ) = cutEnd( L, N );

tuple[Stack,stackelem] takeLabel( [ *L, l:sel( _, _ ) ], 0 ) = < L, l >;
tuple[Stack,stackelem] takeLabel( [ *L, l:sel( _, _ ) ], int N ) = takeLabel( L, N - 1 );
tuple[Stack,stackelem] takeLabel( [ *L, x ], int N ) = takeLabel( L, N );

default config reduce( config c ) {
  println( "reduce rule not implemented" );
  return c;
}

// custom data type
data maybe[&T] = ok( &T val ) | empty( );

// ## Runtime structures

// The integer values are in their unsigned representation
//   Their internal representation is agnostic to signedness, this signedness
//   will be enforced by the operations performed upon them.
data runtime_val = i32( int ival )
                 | i64( int ival )
                 | f32( real fval )
                 | f64( real fval )
                 ;

// The store represents all global state that can be manipulated by WebAssembly
// programs. It consists of the runtime representation of all instances of
// functions, tables, memories, and globals that have been allocated during the
// life time of the abstract machine.
data store = store( list[funcinst] funcs, list[tableinst] tables, list[meminst] mems, list[globalinst] globals );

alias funcaddr = int;
alias tableaddr = int;
alias memaddr = int;
alias globaladdr = int;

data moduleinst = moduleinst( list[FUNCTYPE] types,
                              list[funcaddr] funcaddrs,
                              list[tableaddr] tableaddrs,
                              list[memaddr] memaddrs,
                              list[globaladdr] globaladdrs,
                              list[exportinst] exports );

// TODO?: funcinst should also include host funcs, which are imported.
//        not sure how that would work here. Left it out for now.
data funcinst = funcinst( FUNCTYPE \type, moduleinst \module, FUNC code );

data tableinst = tableinst( list[funcelem] elem, int max )
               | tableinst( list[funcelem] elem )
               ;

data funcelem = funcelem( funcaddr addr )
              | funcelem( )
              ;

data meminst = meminst( list[byte] \data, int maxNumPages )
             | meminst( list[byte] \data )
             ;

data globalinst = globalinst( runtime_val val, MUT mut );

data exportinst = exportinst( /* TODO */ );

// TODO: External Values

alias Stack = list[stackelem];

data CONTROL_INSTR = trap( )
                   | invoke( int addr )
                   | init_elem( /* TODO */ )
                   | init_data( /* TODO */ )
                   | end( ) // pseudo instruction
                   // labels and frames are already on the stack
                   ;

// 'se' stands for 'stackelem'. Shortened for readability because they occur a lot
data stackelem = sev( runtime_val val ) // value
               | sei( INSTR instr ) // instruction
               | sec( CONTROL_INSTR cInstr ) // complementary control instruction
               | sel( int retArity, list[INSTR] instrs ) // label
               | sef( int retArity, frame f ) // frame
               ;

data frame = frame( list[runtime_val] locals, moduleinst \module );

data config = config( store s, thread t );

// Minor deviation from spec. Stack instead of list[INSTR]
data thread = thread( frame baseFrame, Stack stack );

// ## Helper functions

int arity( resulttype( list[VALTYPE] types ) ) = size( types );

runtime_val getlocal( frame( locals, _ ), int idx ) = locals[ idx ];

tuple[frame,Stack] setlocal( frame( locals, m ), stack:[], int lIdx, runtime_val val ) {
  locals[ lIdx ] = val;
  return <frame(locals,m),stack>;
}
tuple[frame,Stack] setlocal( frame baseF, [sef(resArity,frame(locals,m)), *L], int lIdx, runtime_val val ) {
  locals[ lIdx ] = val;
  return <baseF, [ sef(resArity,frame(locals,m)), *L ]>;
}
tuple[frame,Stack] setlocal( frame baseF, [x, *L], int lIdx, runtime_val val )
  = <baseF2, [x, *L2]>
  when <baseF2,L2> := setlocal( baseF, L, lIdx, val );

runtime_val getglobal( store( _, _, _, list[globalinst] globals ), int idx ) = globals[ idx ].val;

store setglobal( s:store( a, b, c, list[globalinst] globals ), int idx, runtime_val val ) {
  globals[ idx ] = globalinst( val, globals[ idx ].mut );
  return store( a, b, c, globals );
}

// Note: In WebAssembly 1.0 'memId' will always be 0.
store setMemoryBytes( store( a, b, list[meminst] mems, c ), int memId, int location, list[byte] values ) {
  for ( int i <- [0..size(values)] ) {
    mems[ memId ].\data[ location + i ] = values[ i ];
  }
  return store( a, b, mems, c );
}

list[byte] getMemoryBytes( store( a, b, list[meminst] mems, c ), int memId, int location, int numBytes )
  = reverse( mem.\data[location..location+numBytes] )
  when meminst mem := mems[ memId ];

int memorySize( store( a, b, list[meminst] mems, c ) ) = size( mems[ 0 ].\data ) / ( 64 * 1024 );

maybe[store] growMemory( store( a, b, list[meminst] mems, c ), int numNewPages ) {
  if ( canGrowMemory( mems[ 0 ], numNewPages ) ) {
    mems[ 0 ] = growMemory( mems[ 0 ], numNewPages );
    return ok( store( a, b, mems, c ) );
  } else {
    return empty( );
  }
}

meminst growMemory( meminst( list[byte] \data, int maxNumPages ), int numNewPages )
  = meminst( \data + [ 0 | i <- [0..numNewPages*(64*1024)] ], maxNumPages );
meminst growMemory( meminst( list[byte] \data ), int numNewPages )
  = meminst( \data + [ 0 | i <- [0..numNewPages*(64*1024)] ] );

bool canGrowMemory( meminst( list[byte] \data, int maxNumPages ), int numNewPages ) = ( currNumPages + numNewPages <= maxNumPages )
  when currNumPages := size( \data ) / ( 64 * 1024 );
bool canGrowMemory( meminst( list[byte] \data ), int numNewPages ) = true; // No max was set

list[funcinst] setupFuncInsts( moduleinst m, list[FUNCTYPE] functypes, list[FUNC] funcs )
  = [ toFuncinst( m, functypes, f ) | f <- funcs ];

funcinst toFuncinst( moduleinst m, list[FUNCTYPE] functypes, f:func( typeIdx, _, _ ) )
  = funcinst( functypes[ typeIdx ], m, f );

list[INSTR] getFuncInstrs( func( _, _, expr( instrs ) ) ) = instrs;

list[meminst] setupMeminsts( list[MEM] mems, list[DATA] \data ) {
  insts = [ toMeminst( m ) | m <- mems ];
  for ( d <- \data ) {
    insts = applyDataToMeminst( insts, d ); 
  }
  return insts;
}

meminst toMeminst( mem( memtype( limits( int min, int max ) ) ) ) = meminst( [ 0 | i <- [0..min*(64*1024)] ], max );
meminst toMeminst( mem( memtype( limits( int min ) ) ) ) = meminst( [ 0 | i <- [0..min*(64*1024)] ] );

list[meminst] applyDataToMeminst( list[meminst] insts, \data( MEMIDX idx, EXPR offset, list[byte] init ) ) {
  int offsetVal = evaluate( offset ).ival;
  meminst inst = insts[ idx ];
  
  // Constraints on the initialisation of memory sections are not fully
  // specified. So, for now, no constraints are enforced or assumed.
  // An exception about too little memory being allocated may be thrown
  // because of this.
  
  for ( i <- [0..size(init)] ) {
    inst.\data[ offsetVal + i ] = init[ i ];
  }
  
  insts[ idx ] = inst;
  return insts;
}

list[tableinst] setupTableinsts( list[TABLE] tables, list[ELEM] elems ) {
  insts = [ setupTableinst( t ) | t <- tables ];
  for ( e <- elems ) {
    insts = applyElemToTableinst( insts, e ); 
  }
  return insts;
}
tableinst setupTableinst( table( tabletype( limits( min, max ), elemType ) ) ) = tableinst( [], max );
tableinst setupTableinst( table( tabletype( limits( min ), elemType ) ) ) = tableinst( [] );

list[tableinst] applyElemToTableinst( list[tableinst] insts, elem( TABLEIDX idx, EXPR offset, list[FUNCIDX] init ) ) {
  int offsetVal = evaluate( offset ).ival;
  tableinst inst = insts[ idx ];
  
  if ( tableinst( _, int max ) := inst && offsetVal + size( init ) > max ) {
    throw AssertionFailed( "Table capacity exceeded" );
  }
  
  if ( size( inst.elem ) < offsetVal + size( init ) ) {
    inst.elem += [ funcelem( ) | i <- [0..(offsetVal + size( init ) - size( inst.elem ))] ];
  }
  
  for ( i <- [0..size( init )] ) {
    inst.elem[ offsetVal + i ] = funcelem( init[ i ] );
  }
  insts[ idx ] = inst;
  return insts;
}

list[globalinst] toGlobalinsts( list[GLOBAL] globals ) = [ toGlobalinst( g ) | g <- globals ];

globalinst toGlobalinst( g:global( globaltype( MUT mut, i32( ) ), EXPR e ) ) = globalinst( evaluate( e ), mut );

runtime_val evaluate( expr( [ i32_const( int ival ) ] ) ) = i32( ival );
runtime_val evaluate( expr( [ i64_const( int ival ) ] ) ) = i64( ival );
runtime_val evaluate( expr( [ f32_const( int fval ) ] ) ) = f32( fval );
runtime_val evaluate( expr( [ f64_const( int fval ) ] ) ) = f64( fval );

FUNCIDX getStartFuncIdx( \start( FUNCIDX funcIdx ) ) = funcIdx;

list[runtime_val] setupFuncLocals( list[FUNCTYPE] types, func( TYPEIDX tIdx, list[VALTYPE] locals, EXPR body ) )
  = setupLocalsFromParams( types[ tIdx ] ) + [ initLocal( localType ) | localType <- locals ];

list[runtime_val] setupLocalsFromParams( functype( list[VALTYPE] params, _ ) ) = [ initLocal( paramType ) | paramType <- params ];
runtime_val initLocal( i32( ) ) = i32( 0 );
runtime_val initLocal( i64( ) ) = i64( 0 );
runtime_val initLocal( f32( ) ) = f32( 0.0 );
runtime_val initLocal( f64( ) ) = f64( 0.0 );

module lang::webassembly::execution::ReductionSimple

import util::Maybe;
import List;

import lang::webassembly::execution::Numerics;
import lang::webassembly::execution::RuntimeStructures;
import lang::webassembly::execution::RuntimeOperations;
import lang::webassembly::ADT;
import util::Float;
import util::Math;
import util::LittleEndian;

// Contains all reduction rules that only modify the stack and instructions, and none of the other
//   properties of the context
//
// Public functions:
// - Maybe[thread] reduce( thread )

// Helpers
public Maybe[thread] reduce( thread( Stack stack, list[instrelem] instructions ) ) = reduce( stack, instructions );

private Maybe[thread] just( Stack stack, list[instrelem] instructions ) = just( thread( stack, instructions ) );

// # values
private Maybe[thread] reduce( S, [ sei( i32_const( ival ) ), *I ] ) = just( [ *S, sev( i32( ival ) ) ], I );
private Maybe[thread] reduce( S, [ sei( i64_const( ival ) ), *I ] ) = just( [ *S, sev( i64( ival ) ) ], I );
private Maybe[thread] reduce( S, [ sei( f32_const( fval ) ), *I ] ) = just( [ *S, sev( f32( clean( 32, fval ) ) ) ], I );
private Maybe[thread] reduce( S, [ sei( f64_const( fval ) ), *I ] ) = just( [ *S, sev( f64( clean( 64, fval ) ) ) ], I );

// # iunop
// clz
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( i32_clz( ) ), *I ] ) = just( [ *S, sev( i32( iclz( 32, ival ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( i64_clz( ) ), *I ] ) = just( [ *S, sev( i64( iclz( 64, ival ) ) ) ], I );
// ctz
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( i32_ctz( ) ), *I ] ) = just( [ *S, sev( i32( ictz( 32, ival ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( i64_ctz( ) ), *I ] ) = just( [ *S, sev( i64( ictz( 64, ival ) ) ) ], I );
// popcnt
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( i32_popcnt( ) ), *I ] ) = just( [ *S, sev( i32( ipopcnt( ival ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( i64_popcnt( ) ), *I ] ) = just( [ *S, sev( i64( ipopcnt( ival ) ) ) ], I );

// # funop
// abs
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_abs( ) ), *I ] ) = just( [ *S, sev( f32( abs( fval ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_abs( ) ), *I ] ) = just( [ *S, sev( f64( abs( fval ) ) ) ], I );
// neg
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_neg( ) ), *I ] ) = just( [ *S, sev( f32( neg( fval ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_neg( ) ), *I ] ) = just( [ *S, sev( f64( neg( fval ) ) ) ], I );
// sqrt
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_sqrt( ) ), *I ] ) = just( [ *S, sev( f32( clean( 32, sqrt( fval ) ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_sqrt( ) ), *I ] ) = just( [ *S, sev( f64( clean( 64, sqrt( fval ) ) ) ) ], I );
// ceil
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_ceil( ) ), *I ] ) = just( [ *S, sev( f32( ceil( fval ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_ceil( ) ), *I ] ) = just( [ *S, sev( f64( ceil( fval ) ) ) ], I );
// floor
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_floor( ) ), *I ] ) = just( [ *S, sev( f32( floor( fval ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_floor( ) ), *I ] ) = just( [ *S, sev( f64( floor( fval ) ) ) ], I );
// trunc
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_trunc( ) ), *I ] ) = just( [ *S, sev( f32( truncf( fval ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_trunc( ) ), *I ] ) = just( [ *S, sev( f64( truncf( fval ) ) ) ], I );
// nearest
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f32_nearest( ) ), *I ] ) = just( [ *S, sev( f32( fnearest( fval ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f64_nearest( ) ), *I ] ) = just( [ *S, sev( f64( fnearest( fval ) ) ) ], I );

// # ibinop
// add
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_add( ) ), *I ] ) = just( [ *S, sev( i32( iadd( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_add( ) ), *I ] ) = just( [ *S, sev( i64( iadd( 64, ival1, ival2 ) ) ) ], I );
// sub: Note that i32s are interally represented as unsigned (non-negative, that is)
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_sub( ) ), *I ] ) = just( [ *S, sev( i32( isub( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_sub( ) ), *I ] ) = just( [ *S, sev( i64( isub( 64, ival1, ival2 ) ) ) ], I );
// mul
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_mul( ) ), *I ] ) = just( [ *S, sev( i32( imul( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_mul( ) ), *I ] ) = just( [ *S, sev( i64( imul( 64, ival1, ival2 ) ) ) ], I );
// div_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( 0 ) ) ], [ sei( i32_div_u( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( 0 ) ) ], [ sei( i64_div_u( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_div_u( ) ), *I ] ) = just( [ *S, sev( i32( idiv_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_div_u( ) ), *I ] ) = just( [ *S, sev( i64( idiv_u( 64, ival1, ival2 ) ) ) ], I );
// div_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( 0 ) ) ], [ sei( i32_div_s( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( 0 ) ) ], [ sei( i64_div_s( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i32( 0x80000000 ) ), sev( i32( 0xFFFFFFFF ) ) ], [ sei( i32_div_s( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i64( 0x8000000000000000 ) ), sev( i64( 0xFFFFFFFFFFFFFFFF ) ) ], [ sei( i64_div_s( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_div_s( ) ), *I ] ) = just( [ *S, sev( i32( idiv_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_div_s( ) ), *I ] ) = just( [ *S, sev( i64( idiv_s( 64, ival1, ival2 ) ) ) ], I );
// rem_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( 0 ) ) ], [ sei( i32_rem_u( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( 0 ) ) ], [ sei( i64_rem_u( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_rem_u( ) ), *I ] ) = just( [ *S, sev( i32( irem_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_rem_u( ) ), *I ] ) = just( [ *S, sev( i64( irem_u( 64, ival1, ival2 ) ) ) ], I );
// rem_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( 0 ) ) ], [ sei( i32_rem_s( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( 0 ) ) ], [ sei( i64_rem_s( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] );
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_rem_s( ) ), *I ] ) = just( [ *S, sev( i32( irem_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_rem_s( ) ), *I ] ) = just( [ *S, sev( i64( irem_s( 64, ival1, ival2 ) ) ) ], I );
// and
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_and( ) ), *I ] ) = just( [ *S, sev( i32( iand( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_and( ) ), *I ] ) = just( [ *S, sev( i64( iand( 64, ival1, ival2 ) ) ) ], I );
// or
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_or( ) ), *I ] ) = just( [ *S, sev( i32( ior( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_or( ) ), *I ] ) = just( [ *S, sev( i64( ior( 64, ival1, ival2 ) ) ) ], I );
// xor
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_xor( ) ), *I ] ) = just( [ *S, sev( i32( ixor( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_xor( ) ), *I ] ) = just( [ *S, sev( i64( ixor( 64, ival1, ival2 ) ) ) ], I );
// shl
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_shl( ) ), *I ] ) = just( [ *S, sev( i32( ishl( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_shl( ) ), *I ] ) = just( [ *S, sev( i64( ishl( 64, ival1, ival2 ) ) ) ], I );
// shr_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_shr_u( ) ), *I ] ) = just( [ *S, sev( i32( ishr_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_shr_u( ) ), *I ] ) = just( [ *S, sev( i64( ishr_u( 64, ival1, ival2 ) ) ) ], I );
// shr_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_shr_s( ) ), *I ] ) = just( [ *S, sev( i32( ishr_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_shr_s( ) ), *I ] ) = just( [ *S, sev( i64( ishr_s( 64, ival1, ival2 ) ) ) ], I );
// rotl
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_rotl( ) ), *I ] ) = just( [ *S, sev( i32( irotl( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_rotl( ) ), *I ] ) = just( [ *S, sev( i64( irotl( 64, ival1, ival2 ) ) ) ], I );
// rotr
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_rotr( ) ), *I ] ) = just( [ *S, sev( i32( irotr( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_rotr( ) ), *I ] ) = just( [ *S, sev( i64( irotr( 64, ival1, ival2 ) ) ) ], I );

// ## fbinop
// add
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_add( ) ), *I ] ) = just( [ *S, sev( f32( clean( 32, add( ival1, ival2 ) ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_add( ) ), *I ] ) = just( [ *S, sev( f64( clean( 64, add( ival1, ival2 ) ) ) ) ], I );
// sub
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_sub( ) ), *I ] ) = just( [ *S, sev( f32( clean( 32, subtract( ival1, ival2 ) ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_sub( ) ), *I ] ) = just( [ *S, sev( f64( clean( 64, subtract( ival1, ival2 ) ) ) ) ], I );
// mul
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_mul( ) ), *I ] ) = just( [ *S, sev( f32( clean( 32, multiply( ival1, ival2 ) ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_mul( ) ), *I ] ) = just( [ *S, sev( f64( clean( 64, multiply( ival1, ival2 ) ) ) ) ], I );
// div
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_div( ) ), *I ] ) = just( [ *S, sev( f32( clean( 32, divide( ival1, ival2 ) ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_div( ) ), *I ] ) = just( [ *S, sev( f64( clean( 64, divide( ival1, ival2 ) ) ) ) ], I );
// min
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_min( ) ), *I ] ) = just( [ *S, sev( f32( min( ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_min( ) ), *I ] ) = just( [ *S, sev( f64( min( ival1, ival2 ) ) ) ], I );
// max
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_max( ) ), *I ] ) = just( [ *S, sev( f32( max( ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_max( ) ), *I ] ) = just( [ *S, sev( f64( max( ival1, ival2 ) ) ) ], I );
// copysign
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_copysign( ) ), *I ] ) = just( [ *S, sev( f32( fcopysign( ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_copysign( ) ), *I ] ) = just( [ *S, sev( f64( fcopysign( ival1, ival2 ) ) ) ], I );

// ## itestop
// eqz
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( i32_eqz( ) ), *I ] ) = just( [ *S, sev( i32( ival == 0 ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( i64_eqz( ) ), *I ] ) = just( [ *S, sev( i32( ival == 0 ? 1 : 0 ) ) ], I );

// ## irelop
// eq
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_eq( ) ), *I ] ) = just( [ *S, sev( i32( ieq( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_eq( ) ), *I ] ) = just( [ *S, sev( i32( ieq( 64, ival1, ival2 ) ) ) ], I );
// ne
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_ne( ) ), *I ] ) = just( [ *S, sev( i32( ine( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_ne( ) ), *I ] ) = just( [ *S, sev( i32( ine( 64, ival1, ival2 ) ) ) ], I );
// lt_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_lt_u( ) ), *I ] ) = just( [ *S, sev( i32( ilt_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_lt_u( ) ), *I ] ) = just( [ *S, sev( i32( ilt_u( 64, ival1, ival2 ) ) ) ], I );
// lt_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_lt_s( ) ), *I ] ) = just( [ *S, sev( i32( ilt_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_lt_s( ) ), *I ] ) = just( [ *S, sev( i32( ilt_s( 64, ival1, ival2 ) ) ) ], I );
// gt_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_gt_u( ) ), *I ] ) = just( [ *S, sev( i32( igt_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_gt_u( ) ), *I ] ) = just( [ *S, sev( i32( igt_u( 64, ival1, ival2 ) ) ) ], I );
// gt_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_gt_s( ) ), *I ] ) = just( [ *S, sev( i32( igt_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_gt_s( ) ), *I ] ) = just( [ *S, sev( i32( igt_s( 64, ival1, ival2 ) ) ) ], I );
// le_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_le_u( ) ), *I ] ) = just( [ *S, sev( i32( ile_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_le_u( ) ), *I ] ) = just( [ *S, sev( i32( ile_u( 64, ival1, ival2 ) ) ) ], I );
// le_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_le_s( ) ), *I ] ) = just( [ *S, sev( i32( ile_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_le_s( ) ), *I ] ) = just( [ *S, sev( i32( ile_s( 64, ival1, ival2 ) ) ) ], I );
// ge_u
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_ge_u( ) ), *I ] ) = just( [ *S, sev( i32( ige_u( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_ge_u( ) ), *I ] ) = just( [ *S, sev( i32( ige_u( 64, ival1, ival2 ) ) ) ], I );
// ge_s
private Maybe[thread] reduce( [ *S, sev( i32( ival1 ) ), sev( i32( ival2 ) ) ], [ sei( i32_ge_s( ) ), *I ] ) = just( [ *S, sev( i32( ige_s( 32, ival1, ival2 ) ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( i64( ival1 ) ), sev( i64( ival2 ) ) ], [ sei( i64_ge_s( ) ), *I ] ) = just( [ *S, sev( i32( ige_s( 64, ival1, ival2 ) ) ) ], I );


// ## frelop
// eq
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_eq( ) ), *I ] ) = just( [ *S, sev( i32( eq( ival1, ival2 ) ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_eq( ) ), *I ] ) = just( [ *S, sev( i32( eq( ival1, ival2 ) ? 1 : 0 ) ) ], I );
// ne
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_ne( ) ), *I ] ) = just( [ *S, sev( i32( ne( ival1, ival2 ) ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_ne( ) ), *I ] ) = just( [ *S, sev( i32( ne( ival1, ival2 ) ? 1 : 0 ) ) ], I );
// lt
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_lt( ) ), *I ] ) = just( [ *S, sev( i32( lt( ival1, ival2 ) ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_lt( ) ), *I ] ) = just( [ *S, sev( i32( lt( ival1, ival2 ) ? 1 : 0 ) ) ], I );
// gt
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_gt( ) ), *I ] ) = just( [ *S, sev( i32( gt( ival1, ival2 ) ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_gt( ) ), *I ] ) = just( [ *S, sev( i32( gt( ival1, ival2 ) ? 1 : 0 ) ) ], I );
// le
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_le( ) ), *I ] ) = just( [ *S, sev( i32( le( ival1, ival2 ) ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_le( ) ), *I ] ) = just( [ *S, sev( i32( le( ival1, ival2 ) ? 1 : 0 ) ) ], I );
// ge
private Maybe[thread] reduce( [ *S, sev( f32( ival1 ) ), sev( f32( ival2 ) ) ], [ sei( f32_ge( ) ), *I ] ) = just( [ *S, sev( i32( ge( ival1, ival2 ) ? 1 : 0 ) ) ], I );
private Maybe[thread] reduce( [ *S, sev( f64( ival1 ) ), sev( f64( ival2 ) ) ], [ sei( f64_ge( ) ), *I ] ) = just( [ *S, sev( i32( ge( ival1, ival2 ) ? 1 : 0 ) ) ], I );

// ## cvtop
// i32_wrap_i64: Convert a 64-bit integer to a 32-bit integer
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( i32_wrap_i64( ) ), *I ] ) = just( [ *S, sev( i32( ival % pow2( 32 ) ) ) ], I );
// i64_extend_u_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( i64_extend_u_i32( ) ), *I ] ) = just( [ *S, sev( i64( ival ) ) ], I );
// i64_extend_s_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( i64_extend_s_i32( ) ), *I ] ) = just( [ *S, sev( i64( invSigned( 64, signed( 32, ival ) ) ) ) ], I );
// i32_trunc_u_f32
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i32_trunc_u_f32( ) ), *I ] ) = just( [ *S, sev( i32( invSigned( 32, res ) ) ) ], I )
  when just( res ) := trunc_u( 32, fval );
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i32_trunc_u_f32( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i32_trunc_s_f32
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i32_trunc_s_f32( ) ), *I ] ) = just( [ *S, sev( i32( invSigned( 32, res ) ) ) ], I )
  when just( res ) := trunc_s( 32, fval );
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i32_trunc_s_f32( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i32_trunc_u_f64
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i32_trunc_u_f64( ) ), *I ] ) = just( [ *S, sev( i32( invSigned( 32, res ) ) ) ], I )
  when just( res ) := trunc_u( 32, fval );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i32_trunc_u_f64( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i32_trunc_s_f64
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i32_trunc_s_f64( ) ), *I ] ) = just( [ *S, sev( i32( invSigned( 32, res ) ) ) ], I )
  when just( res ) := trunc_s( 32, fval );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i32_trunc_s_f64( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i64_trunc_u_f32
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i64_trunc_u_f32( ) ), *I ] ) = just( [ *S, sev( i64( invSigned( 64, res ) ) ) ], I )
  when just( res ) := trunc_u( 64, fval );
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i64_trunc_u_f32( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i64_trunc_s_f32
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i64_trunc_s_f32( ) ), *I ] ) = just( [ *S, sev( i64( invSigned( 64, res ) ) ) ], I )
  when just( res ) := trunc_s( 64, fval );
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i64_trunc_s_f32( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i64_trunc_u_f64
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i64_trunc_u_f64( ) ), *I ] ) = just( [ *S, sev( i64( invSigned( 64, res ) ) ) ], I )
  when just( res ) := trunc_u( 64, fval );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i64_trunc_u_f64( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// i64_trunc_s_f64
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i64_trunc_s_f64( ) ), *I ] ) = just( [ *S, sev( i64( invSigned( 64, res ) ) ) ], I )
  when just( res ) := trunc_s( 64, fval );
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i64_trunc_s_f64( ) ), *I ] ) = just( [ ], [ sec( trap( ) ) ] );
// f32_demote_f64
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( f32_demote_f64( ) ), *I ] ) = just( [ *S, sev( f32( fval ) ) ], I );
// f64_promote_f32
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( f64_promote_f32( ) ), *I ] ) = just( [ *S, sev( f64( fval ) ) ], I );
// f32_convert_u_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( f32_convert_u_i32( ) ), *I ] ) = just( [ *S, sev( f32( fval( toReal( ival ) ) ) ) ], I );
// f32_convert_s_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( f32_convert_s_i32( ) ), *I ] ) = just( [ *S, sev( f32( fval( toReal( signed( 32, ival ) ) ) ) ) ], I );
// f32_convert_u_i64
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( f32_convert_u_i64( ) ), *I ] ) = just( [ *S, sev( f32( fval( toReal( ival ) ) ) ) ], I );
// f32_convert_s_i64
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( f32_convert_s_i64( ) ), *I ] ) = just( [ *S, sev( f32( fval( toReal( signed( 64, ival ) ) ) ) ) ], I );
// f64_convert_u_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( f64_convert_u_i32( ) ), *I ] ) = just( [ *S, sev( f64( fval( toReal( ival ) ) ) ) ], I );
// f64_convert_s_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( f64_convert_s_i32( ) ), *I ] ) = just( [ *S, sev( f64( fval( toReal( signed( 32, ival ) ) ) ) ) ], I );
// f64_convert_u_i64
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( f64_convert_u_i64( ) ), *I ] ) = just( [ *S, sev( f64( fval( toReal( ival ) ) ) ) ], I );
// f64_convert_s_i64
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( f64_convert_s_i64( ) ), *I ] ) = just( [ *S, sev( f64( fval( toReal( signed( 64, ival ) ) ) ) ) ], I );
// i32_reinterpret_f32
private Maybe[thread] reduce( [ *S, sev( f32( fval ) ) ], [ sei( i32_reinterpret_f32( ) ), *I ] ) = just( [ *S, sev( i32( intLE( toBytes( 4, fval ) ) ) ) ], I );
// i64_reinterpret_f64
private Maybe[thread] reduce( [ *S, sev( f64( fval ) ) ], [ sei( i64_reinterpret_f64( ) ), *I ] ) = just( [ *S, sev( i64( intLE( toBytes( 8, fval ) ) ) ) ], I );
// f32_reinterpret_i32
private Maybe[thread] reduce( [ *S, sev( i32( ival ) ) ], [ sei( f32_reinterpret_i32( ) ), *I ] ) = just( [ *S, sev( f32( toFloat( bytesLE( 4, ival ) ) ) ) ], I );
// f64_reinterpret_i64
private Maybe[thread] reduce( [ *S, sev( i64( ival ) ) ], [ sei( f64_reinterpret_i64( ) ), *I ] ) = just( [ *S, sev( f64( toFloat( bytesLE( 8, ival ) ) ) ) ], I );

// ## parametric instructions
// drop( ): Drop top value from the stack
private Maybe[thread] reduce( [ *S, sev( _ ) ], [ sei( drop( ) ), *I ] ) = just( S, I );
// select( ): If c != 0 (thus, true), push first, otherwise push second - similar to conditional ( c ? val1 : val2 )
private Maybe[thread] reduce( [ *S, sev( v1 ), sev( v2 ), sev( i32( 0 ) ) ], [ sei( select( ) ), *I ] ) = just( [ *S, sev( v2 ) ], I  );
private Maybe[thread] reduce( [ *S, sev( v1 ), sev( v2 ), sev( i32( _ ) ) ], [ sei( select( ) ), *I ] ) = just( [ *S, sev( v1 ) ], I );

// ## control instructions
// nop
private Maybe[thread] reduce( S, [ sei( nop( ) ), *I ] ) = just( S, I );
// unreachable
private Maybe[thread] reduce( S, [ sei( unreachable( ) ), *I ] ) = just( [], [ sec( trap( ) ) ] ); // Made shorter than definition
// block
private Maybe[thread] reduce( S, [ sei( block( restype, instrs ) ), *I ] ) = just( [ *S, sel( arity( restype ), [] ) ], [ sei( i ) | i <- instrs ] + sec( end( ) ) + I );
// loop
private Maybe[thread] reduce( S, [ sei( l:loop( restype, instrs ) ), *I ] ) = just( [ *S, sel( /* arity( restype ) */ 0, [ l ] ) ], [ sei( i ) | i <- instrs ] + sec( end( ) ) + I );
// if
private Maybe[thread] reduce( [ *S, sev( i32( 0 ) ) ], [ sei( \if( restype, ifInstrs, elseInstrs ) ), *I ] ) = just( [ *S, sel( arity( restype ), [] ) ], [ sei( i ) | i <- elseInstrs ] + sec( end( ) ) + I );
private Maybe[thread] reduce( [ *S, sev( i32( _ ) ) ], [ sei( \if( restype, ifInstrs, elseInstrs ) ), *I ] ) = just( [ *S, sel( arity( restype ), [] ) ], [ sei( i ) | i <- ifInstrs ] + sec( end( ) ) + I );
// call( )
private Maybe[thread] reduce( S, [ sei( call( FUNCIDX fi ) ), *I ] ) = just( S, [ sec( invoke( fi ) ), *I ] );
// br_if
private Maybe[thread] reduce( [ *S, sev( i32( 0 ) ) ], [ sei( br_if( LABELIDX idx ) ), *I ] ) = just( S, I );
private Maybe[thread] reduce( [ *S, sev( i32( c ) ) ], [ sei( br_if( LABELIDX idx ) ), *I ] ) = just( S, [ sei( br( idx ) ), *I ] );
// br_table
private Maybe[thread] reduce( [ *S, sev( i32( i ) ) ], [ sei( br_table( labelIdxs, defaultLabelIdx ) ), *I ] ) = just( S, [ sei( br( labelIdx ) ), *I ] )
  when labelIdx := ( i < size( labelIdxs ) ? labelIdxs[ i ] : defaultLabelIdx );

private default Maybe[thread] reduce( Stack s, list[instrelem] instructions ) = nothing( );

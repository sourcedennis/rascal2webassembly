module lang::webassembly::execution::Numerics

import IO; // temp
import List;
import util::IntPower;
import Exception;

// ## Integers

public int signed( int N, int i ) {
  if ( 0 <= i && i < pow2( N - 1 ) ) {
    return i;
  } else if ( pow2( N - 1 ) <= i && i < pow2( N ) ) {
    return i - pow2( N );
  } else {
    throw IllegalArgument( "Out of range <i>" );
  }
}

public int invSigned( int N, int i ) {
  // Strictly: 0 <= i && i < pow2( N - 1 )
  // However, the int may already be in unsigned format
  if ( 0 <= i && i < pow2( N ) ) {
    return i;
  } else if ( -pow2( N - 1 ) <= i && i < 0 ) {
    return i + pow2( N );
  } else {
    throw IllegalArgument( "Out of range <i>" );
  }
}

public alias UInt = int;

public UInt iadd( int N, UInt a, UInt b ) = ( a + b ) % pow2( N );
public UInt isub( int N, UInt a, UInt b ) = ( a - b + pow2( N ) ) % pow2( N );
public UInt imul( int N, UInt a, UInt b ) = ( a * b ) % pow2( N );
public UInt idiv_u( int N, UInt a, UInt b:0 ) = 0; // undefined behaviour
public UInt idiv_u( int N, UInt a, UInt b ) = a / b; // truncate toward 0 (integer division)
public UInt idiv_s( int N, UInt a, UInt b:0 ) = 0; // undefined behaviour
public UInt idiv_s( int N, UInt a, UInt b ) = ( res == pow2( N - 1 ) ? 0 : invSigned( N, j1 / j2 ) )
  when j1 := signed( N, a ), j2 := signed( N, b ), res := j1 / j2;

// iclz: number of leading 0 bits
public UInt iclz( int N, UInt val:0 ) = N;
public UInt iclz( int N:0, UInt val ) = 0;
public UInt iclz( int N, UInt val ) = iclz( N - 1, val / 2 );

// ictz: number of trailing 0 bits
public UInt ictz( int N, UInt val:0 ) = N;
public UInt ictz( int N, UInt val ) = ( val % 2 == 0 ? ictz( N - 1, val / 2 ) + 1 : 0 );

// ipopcnt: number of 1 bits
public UInt ipopcnt( UInt val:0 ) = 0;
public UInt ipopcnt( UInt val ) = ( val % 2 ) + ipopcnt( val / 2 );

public UInt irem_u( int N, UInt i1, UInt i2:0 ) = 0;
public UInt irem_u( int N, UInt i1, UInt i2 ) = i1 - i2 * ( i1 / i2 );

public UInt irem_s( int N, UInt i1, UInt i2:0 ) = 0;
public UInt irem_s( int N, UInt i1, UInt i2 ) = invSigned( N, j1 - j2 * ( j1 / j2 ) )
  when j1 := signed( N, i1 ), j2 := signed( N, i2 );

public UInt iand( int N, UInt i1, UInt i2 ) = binaryOp( N, and, i1, i2 );
public UInt ior( int N, UInt i1, UInt i2 ) = binaryOp( N, or, i1, i2 );
public UInt ixor( int N, UInt i1, UInt i2 ) = binaryOp( N, xor, i1, i2 );

public UInt ishl( int N, UInt i1, UInt i2 )
  = invIbits( i1bits[k..] + [ 0 | i <- [0..k] ] )
  when k := i2 % N,
       list[bit] i1bits := ibits( N, i1 );

public UInt ishr_u( int N, UInt i1, UInt i2 )
  = invIbits( i1bits[..N-k] )
  when k := i2 % N,
       list[bit] i1bits := ibits( N, i1 );

public UInt ishr_s( int N, UInt i1, UInt i2 )
  = invIbits( [ d0 | i <- [0..k+1] ] + i1bits[1..N-k] )
  when k := i2 % N,
       list[bit] i1bits := ibits( N, i1 ),
       d0 := head( i1bits );

public UInt irotl( int N, UInt i1, UInt i2 )
  = invIbits( i1bits[k..] + i1bits[..k] )
  when k := i2 % N, i1bits := ibits( N, i1 );

public UInt irotr( int N, UInt i1, UInt i2 )
  = invIbits( i1bits[N-k..] + i1bits[..N-k] )
  when k := i2 % N, i1bits := ibits( N, i1 );

public int ieqz( int N, UInt i ) = ( i == 0 ) ? 1 : 0;
public int ieq( int N, UInt i1, UInt i2 ) = ( i1 == i2 ) ? 1 : 0;
public int ine( int N, UInt i1, UInt i2 ) = ( i1 != i2 ) ? 1 : 0;
public int ilt_u( int N, UInt i1, UInt i2 ) = ( i1 < i2 ) ? 1 : 0;
public int ilt_s( int N, UInt i1, UInt i2 ) = ( signed( N, i1 ) < signed( N, i2 ) ) ? 1 : 0;
public int igt_u( int N, UInt i1, UInt i2 ) = ( i1 > i2 ) ? 1 : 0;
public int igt_s( int N, UInt i1, UInt i2 ) = ( signed( N, i1 ) > signed( N, i2 ) ) ? 1 : 0;
public int ile_u( int N, UInt i1, UInt i2 ) = ( i1 <= i2 ) ? 1 : 0;
public int ile_s( int N, UInt i1, UInt i2 ) = ( signed( N, i1 ) <= signed( N, i2 ) ) ? 1 : 0;
public int ige_u( int N, UInt i1, UInt i2 ) = ( i1 >= i2 ) ? 1 : 0;
public int ige_s( int N, UInt i1, UInt i2 ) = ( signed( N, i1 ) >= signed( N, i2 ) ) ? 1 : 0;

public alias bit = int;
public alias bits = list[bit];
public alias byte = int;
public alias bytes = list[byte];

public bits ibits( int N:0, UInt a ) = [];
public bits ibits( int N, UInt a ) = ibits( N - 1, a / 2 ) + [ a % 2 ];
public UInt invIbits( bits _:[] ) = 0;
public UInt invIbits( bits _:[*bs, bit b] ) = invIbits( bs ) * 2 + b;

// ## Floating Points
// Most of these are moved to util::Float

// ## Helpers

private bit and( bit a, bit b ) = ( ( a == 1 && b == 1 ) ? 1 : 0 );
private bit or( bit a, bit b ) = ( ( a == 1 || b == 1 ) ? 1 : 0 );
private bit xor( bit a, bit b ) = ( ( a != b ) ? 1 : 0 );

private UInt binaryOp( int N, bit(bit,bit) op, UInt i1, UInt i2 ) = invIbits( [ op( a, b ) | <a,b> <- zip( ibits( N, i1 ), ibits( N, i2 ) ) ] );

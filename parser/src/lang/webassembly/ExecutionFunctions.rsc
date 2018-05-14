module lang::webassembly::ExecutionFunctions

import List;
import util::Math;
import IO; // temp

int pow2( int N ) = toInt( pow( 2, N ) );
// Signed interpretation
int signed( int a, int N ) {
  if ( a < pow2( N - 1 ) ) {
    return a;
  } else {
    return a - pow2( N );
  }
}
int invSigned( int a, int N ) {
  if ( a >= 0 ) {
    return a;
  } else {
    return a + pow2( N );
  }
}

alias bit = int;
alias byte = int;

list[bit] ibits( int a, 0 ) = [];
list[bit] ibits( int a, int N ) = ibits( a / 2, N - 1 ) + [ a % 2 ];
int invIbits( [] ) = 0;
int invIbits( [*bits, bit b] ) = invIbits( bits ) * 2 + b;

// Note that these are little-endian, as defined by the WebAssembly spec
list[byte] toBytes( [] ) = [];
list[byte] toBytes( [*bits, i7, i6, i5, i4, i3, i2, i1, i0] ) = toByte( [i7, i6, i5, i4, i3, i2, i1, i0] ) + toBytes( bits );
byte toByte( [] ) = 0;
byte toByte( [*L, b] ) = toByte( L ) * 2 + b;
list[byte] toBytes( int val, int N ) = toBytes( ibits( val, N ) );
list[byte] toBytes( real val, int N ) = toBytes( fbits( val, N ) );

list[bit] toBits( list[byte] bs ) = [ x | b <- bs, x <- toBits( b, 8 ) ];
list[bit] toBits( _, 0 ) = [];
list[bit] toBits( byte b, int N ) = toBits( b / 2, N - 1 ) + [ b % 2 ];

int signif( 32 ) = 23;
int signif( 64 ) = 52;
int expon( 32 ) = 8;
int expon( 64 ) = 11;
int fbias( int N ) = pow2( expon( N ) - 1 ) - 1;

list[bit] fbits( real f, int N ) {
  // TODO: Not fully formal
  int exp = 0;
  if ( f * 2 < pow2( signif( N ) ) ) {
    while ( f != toInt( f ) && f * 2 < pow2( signif( N ) ) ) {
      exp = exp - 1;
      f = f * 2;
    }
  } else {
    while ( f / 2 >= pow2( signif( N ) ) ) {
      exp = exp + 1;
      f = f / 2;
    }
  }
  int m = abs( toInt( f ) );
  return [ f >= 0 ? 0 : 1 ] + ibits( exp + fbias( N ), expon( N ) ) + ibits( m, signif( N ) );
}

real invFbits( list[bit] bits ) {
  // TODO: Not fully formal
  int N = size( bits );
  int sign = bits[0];
  int exp = invIbits( bits[1..1+expon(N)] ) - fbias( N );
  int m = invIbits( bits[1+expon(N)..] );
  return ( sign == 0 ? 1 : -1 ) * ( m * pow( 2.0, exp ) );
}

// ### Function implementations

// iclz: number of leading 0 bits
int iclz( 0, int N ) = N;
int iclz( int val, 0 ) = 0;
int iclz( int val, int N ) = iclz( val / 2, N - 1 );
// ictz: number of trailing 0 bits
int ictz( 0, int N ) = N;
int ictz( int val, int N ) {
  if ( val % 2 == 0 ) {
    return 1 + ictz( val / 2, 0 );
  } else {
    // This is the recursion's base case. ictz( 0, N ) is an edge case
    return 0;
  }
}
// ipopcnt: number of 1 bits in
int ipopcnt( 0 ) = 0;
int ipopcnt( int val ) = ( val % 2 ) + ipopcnt( val / 2 );

real truncf( real f ) {
  int ival = toInt( f );
  if ( ival == 0 ) {
    if ( f >= 0 ) {
      return 0.0;
    } else {
      return -0.0;
    }
  } else {
    return toReal( ival );
  }
}

real fnearest( real f ) {
  int ival = toInt( f );
  real frac = f - ival;
      
  if ( frac < 0.5 ) {
    return toReal( ival );
  } else if ( frac > 0.5 ) {
    return toReal( ival + 1 );
  } else { // if two values are equally near, return the even one
    if ( ival % 2 == 0 ) {
      return toReal( ival );
    } else {
      return toReal( ival + 1 );
    }
  }
}

int idiv_u( int a, int b, int N ) {
  if ( b == 0 ) {
    return 0; // undefined behaviour
  } else {
    return a / b; // truncate toward 0 (integer division)
  }
}

int idiv_s( int a, int b, int N ) {
  if ( b == 0 ) {
    return 0; // undefined behaviour
  } else {
    return invSigned( signed( a, N ) / signed( b, N ), N ); // truncate toward 0 (integer division)
  }
  // TODO: Formally there is an undefined case missing
}

int irem_u( int i1, int i2, int N ) {
  if ( i2 == 0 ) {
    return 0; // undefined behaviour
  } else {
    return i1 - i2 * ( i1 / i2 );
  }
}

int irem_s( int i1, int i2, int N ) {
  if ( i2 == 0 ) {
    return 0; // undefined behaviour
  } else {
    int j1 = signed( i1 );
    int j2 = signed( i2 );
    return invSigned( j1 - j2 * ( j1 / j2 ) );
  }
}

bit and( bit a, bit b ) = ( ( a == 1 && b == 1 ) ? 1 : 0 );
bit or( bit a, bit b ) = ( ( a == 1 || b == 1 ) ? 1 : 0 );
bit xor( bit a, bit b ) = ( ( a != b ) ? 1 : 0 );

int iand( int i1, int i2, int N ) = invIbits( [ and( a, b ) | <a,b> <- zip( ibits( i1, N ), ibits( i2, N ) ) ] );
int ior( int i1, int i2, int N ) = invIbits( [ or( a, b ) | <a,b> <- zip( ibits( i1, N ), ibits( i2, N ) ) ] );
int ixor( int i1, int i2, int N ) = invIbits( [ xor( a, b ) | <a,b> <- zip( ibits( i1, N ), ibits( i2, N ) ) ] );

int ishl( int i1, int i2, int N )
  = invIbits( i1bits[k..] + [ 0 | i <- [0..k] ] )
  when k := i2 % N,
       list[bit] i1bits := ibits( i1, N );

int ishr_u( int i1, int i2, int N )
  = invIbits( i1bits[..N-k] )
  when k := i2 % N,
       list[bit] i1bits := ibits( i1, N );

int ishr_s( int i1, int i2, int N )
  = invIbits( [ d0 | i <- [0..k+1] ] + i1bits[1..N-k] )
  when k := i2 % N,
       list[bit] i1bits := ibits( i1, N ),
       d0 := head( i1bits );

int rotl( int i1, int i2, int N )
  = invIbits( i1bits[k..] + i1bits[..k] )
  when k := i2 % N,
       i1bits := ibits( i1, N );

int rotr( int i1, int i2, int N )
  = invIbits( i1bits[N-k..] + i1bits[..N-k] )
  when k := i2 % N,
       i1bits := ibits( i1, N );

// copies i2's sign to i1
real fcopysign( real i1, real i2 ) {
  if ( ( i1 >= 0 ) == ( i2 >= 0 ) ) {
    return i1;
  } else {
    return -i1;
  }
}

int trunc_u( real f, int destN ) {
  int i = toInt( f );
  if ( i >= pow2( destN ) ) {
    return 0; // undefined
  } else {
    return i;
  }
}

int trunc_s( real f, int destN ) {
  int i = toInt( f );
  if ( i >= pow2( destN - 1 ) || i < -pow2( destN - 1 ) ) {
    return 0; // undefined
  } else {
    return i;
  }
}

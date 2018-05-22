module lang::webassembly::Operations

/*import List;
import util::Math;
import lang::webassembly::Float;*/

/*int pow2( int N ) = toInt( pow( 2, N ) );
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
list[bit] toBits( byte b, int N ) = toBits( b / 2, N - 1 ) + [ b % 2 ];*/
module lang::webassembly::util::ToFloat

import ParseTree;
import String;
import util::Math;

import lang::webassembly::util::FloatSyntax;
import lang::webassembly::util::ToInt;
import util::Float;

// This converts from WebAssembly's float syntax to
//   the floating point implementation

public Float toFloat( str s ) {
  FN tree = parse( #FN, s );
  
  switch ( tree ) {
  case (FN)`<SN sn>`: return fval( toReal( toIntWasm( "<sn>" ) ) ); // this is a normal decimal/hexadecimal integer
  case (FN)`-inf`: return negative_infinity( );
  case (FN)`+inf`: return positive_infinity( );
  case (FN)`inf`: return positive_infinity( );
  case (FN)`<Sign _>nan`: return canonical_nan( );
  case (FN)`<Sign _><ArithmeticNan _>`: return arithmetic_nan( );
  case (FN)`+<FNMag x>`: return toFloat( x );
  case (FN)`<FNMag x>`: return toFloat( x );
  case (FN)`-<FNMag x>`: {
    if ( fval( v ) := toFloat( x ) ) {
      return fval( -v );
    }
  }
  };
  
  println( "Failed to parse Float" );
  return arithmetic_nan( );
}

private Float toFloat( (FNMag)`<Float f>` ) = fval( toReal( replaceAll( "<f>", "_", "" ) ) );
private Float toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>` ) {
  magnitude = toReal( n ) + toFrac( f );
  return fval( magnitude );
}
private Float toFloat( (FNMag)`0x<HexNum n>P<Sign s><Num p>` ) = toFloat( (FNMag)`0x<HexNum n>p<Sign s><Num p>` );
private Float toFloat( (FNMag)`0x<HexNum n>p<Sign s><Num p>` ) {
  magnitude = toReal( n );
  power = toIntWasm( "<s><p>" );
  return fval( applyPower( magnitude, power ) );
}
private Float toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>P<Sign s><Num p>` ) = toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>p<Sign s><Num p>` );
private Float toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>p<Sign s><Num p>` ) {
  magnitude = toReal( n ) + toFrac( f );
  power = toIntWasm( "<s><p>" );
  return fval( applyPower( magnitude, power ) );
}

private real toReal( HexNum n ) = toReal( toIntWasm( "0x<n>" ) );

// This function is introduced to avoid rounding errors from "magnitude * pow( 2.0, power )"
private real applyPower( real magnitude, int power ) {
  if ( power > 0 ) {
    for ( i <- [0..power] ) {
      magnitude = magnitude * 2;
    }
  } else if ( power < 0 ) {
    for ( i <- [0..-power] ) {
      magnitude = magnitude * 0.5;
    }
  }
  return magnitude;
}

// This function is introduced to avoid rounding errors from "f / pow(16.0, size(f))"
private real toFrac( HexFrac f )
  = ( size( fStr ) == 0 ? 0.0 : toFrac( split( "", fStr ) ) )
  when fStr := replaceAll( "<f>", "_", "" );
private real toFrac( list[str] x:[c, *L] ) = ( 1.0 / 16.0 ) * ( toReal( toInt( "0x<c>" ) ) + toFrac( L ) );
private real toFrac( list[str] x:[] ) = 0.0;

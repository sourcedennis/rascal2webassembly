module lang::webassembly::ToFloat

import ParseTree;
import String;
import util::Math;

import lang::webassembly::FloatSyntax;
import util::Float;

Float toFloat( str s ) {
  FN tree = parse( #FN, s );
  
  switch ( tree ) {
  case (FN)`<SN sn>`: return fval( toReal( toInt( sn ) ) ); // this is a normal decimal/hexadecimal integer
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

str cleanIntStr( "-0" ) = "0"; // toInt("-0") gives an exception
str cleanIntStr( str s ) = s;
int toInt( SN sn ) = toInt( cleanIntStr( replaceAll( "<sn>", "_", "" ) ) );

Float toFloat( (FNMag)`<Float f>` ) = fval( toReal( replaceAll( "<f>", "_", "" ) ) );
Float toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>` ) {
  magnitude = toReal( n ) + toFrac( f );
  return fval( magnitude );
}
Float toFloat( (FNMag)`0x<HexNum n>P<Sign s><Num p>` ) = toFloat( (FNMag)`0x<HexNum n>p<Sign s><Num p>` );
Float toFloat( (FNMag)`0x<HexNum n>p<Sign s><Num p>` ) {
  magnitude = toReal( n );
  power = toInt( replaceAll( "<s><p>", "_", "" ) );
  return fval( applyPower( magnitude, power ) );
}
Float toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>P<Sign s><Num p>` ) = toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>p<Sign s><Num p>` );
Float toFloat( (FNMag)`0x<HexNum n>.<HexFrac f>p<Sign s><Num p>` ) {
  magnitude = toReal( n ) + toFrac( f );
  power = toInt( replaceAll( "<s><p>", "_", "" ) );
  return fval( applyPower( magnitude, power ) );
}

real toReal( HexNum n ) = toReal( toInt( replaceAll( "0x<n>", "_", "" ) ) );

// This function is introduced to avoid rounding errors from "magnitude * pow( 2.0, power )"
real applyPower( real magnitude, int power ) {
  if ( power > 0 ) {
    for ( i <- [0..power] ) {
      magnitude = magnitude * 2;
    }
  } else if ( power < 0 ) {
    for ( i <- [0..-power] ) {
      magnitude = magnitude / 2;
    }
  }
  return magnitude;
}

// This function is introduced to avoid rounding errors from "f / pow(16.0, size(f))"
real toFrac( HexFrac f )
  = ( size( fStr ) == 0 ? 0.0 : toFrac( split( "", fStr ) ) )
  when fStr := replaceAll( "<f>", "_", "" );
real toFrac( list[str] x:[c, *L] ) = ( 1.0 / 16.0 ) * ( toReal( toInt( "0x<c>" ) ) + toFrac( L ) );
real toFrac( list[str] x:[] ) = 0.0;
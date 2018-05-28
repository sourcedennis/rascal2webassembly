module util::IntPower

// Returns 2^power
public int pow2( int power ) = pow( 2, power );

// Introduced because util::Math's 'pow' has inaccuracies for high powers
public int pow( int base, int power:0 ) = 1;
public int pow( int base, int power ) {
  assert power > 0;
  if ( power % 2 == 0 ) {
    return pow( base * base, power / 2 );
  } else {
    return base * pow( base, power - 1 );
  }
}

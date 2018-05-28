module lang::webassembly::ToInt

import String;

// Function to converts a WASM integer literal, to Rascal's integer value

public int toIntWasm( str s ) = toInt( cleanSign( cleanOct( cleanSugar( s ) ) ) );

// toInt("-0") gives an exception
private str cleanSign( "-0" ) = "0";
private str cleanSign( str s ) = s;

// WebAssembly does not support octal integer syntax.
// So, e.g. 042 == 42 decimal
private str cleanOct( /^<sign:[-+]?>0*<val:[1-9]\d*>$/ ) = sign + val;
private str cleanOct( str s ) = s;

private str cleanSugar( str s ) = replaceAll( s, "_", "" );
module lang::webassembly::util::String2UTF8

import ParseTree;
import lang::webassembly::util::StringSyntax;
import String;

// By the WebAssembly spec, strings are lexical entities
// However, they need to be converted to UTF-8 byte sequences
// for their binary representation in the ADT. To facilitate this
// conversion these functions are introduced.

public alias byte = int;
public list[byte] wasmStringToUTF8Bytes( str s ) {
  String tree = parse( #String, s );
  list[byte] utf8Bytes = [];
  top-down-break visit ( tree ) {
  case StringElem e: {
    utf8Bytes += toUTF8Bytes( e );
  }
  }
  return utf8Bytes;
}

// Note that unicode characters are not preserved, instead they are
//   replaced by their binary (hexadecimal) representation
//   as within the binary representation the initial format is lost
//   This representation is binary equivalent, though.
public str bytesToWasmString( list[byte] bs ) {
  str s = "";
  for ( b <- bs ) {
    if ( b == 0x22 ) { // double quote
      s += "\\\"";
    } else if ( b >= 0x20 && b < 0x7E ) {
      // printable ascii character
      s += fromPayloadChar( stringChar( b ) );
    } else {
      s += "\\" + toHex2( b );
    }
  }
  return "\"<s>\"";
}

public int wasmStringUTF8Length( str s ) {
  String tree = parse( #String, s );
  int len = 0;
  visit ( tree ) {
  case StringElem e: len += utf8Length( e );
  }
  return len;
}

// A string in WASM format (with quotes) to the string it represents
public str toPayload( str s ) {
  String tree = parse( #String, s );
  str payload = "";
  top-down-break visit ( tree ) {
  case StringElem e: payload += toPayload( e );
  }
  return payload;
}

public str fromPayload( str s ) {
  str out = "";
  for ( c <- chars( s ) ) {
    if ( c < 0x20 || ( c >= 0x7F && c <= 0xFF ) ) {
      out += "\\" + toHex2( c );
    } else {
      out += fromPayloadChar( stringChars( [ c ] ) );
    }
  }
  return "\"<out>\"";
}

private list[byte] toUTF8Bytes( (StringElem)`\\t` ) = [ charAt( "\t", 0 ) ];
private list[byte] toUTF8Bytes( (StringElem)`\\n` ) = [ charAt( "\n", 0 ) ];
private list[byte] toUTF8Bytes( (StringElem)`\\r` ) = [ charAt( "\r", 0 ) ];
private list[byte] toUTF8Bytes( (StringElem)`\\"` ) = [ charAt( "\"", 0 ) ];
private list[byte] toUTF8Bytes( (StringElem)`\\'` ) = [ charAt( "\'", 0 ) ];
private list[byte] toUTF8Bytes( (StringElem)`\\\\` ) = [ charAt( "\\", 0 ) ];
private list[byte] toUTF8Bytes( (StringElem)`<HexEscape e>` ) = unicodeCharToUTF8( hex2int( substring( "<e>", 2 ) ) );
private list[byte] toUTF8Bytes( (StringElem)`\\<HexDigit d1><HexDigit d2>` ) = [ toInt( "0x<d1><d2>" ) ];
private default list[byte] toUTF8Bytes( StringElem e ) = unicodeCharToUTF8( charAt( "<e>", 0 ) );

private str toHex2( int i ) {
  int leastSignif = i % 0x10;
  int mostSignif  = ( i / 0x10 ) % 0x10;
  return "<toHexChar(mostSignif)><toHexChar(leastSignif)>";
}

private str toHexChar( int i ) {
  assert i >= 0 && i < 0x10;
  
  if ( i <= 0x9 ) {
    return "<i>";
  } else {
    switch ( i ) {
    case 0xA: return "A";
    case 0xB: return "B";
    case 0xC: return "C";
    case 0xD: return "D";
    case 0xE: return "E";
    case 0xF: return "F";
    }
  }
}

private str toPayload( (StringElem)`\\t` ) = "\t";
private str toPayload( (StringElem)`\\n` ) = "\n";
private str toPayload( (StringElem)`\\r` ) = "\r";
private str toPayload( (StringElem)`\\"` ) = "\"";
private str toPayload( (StringElem)`\\'` ) = "\'";
private str toPayload( (StringElem)`\\\\` ) = "\\";
private str toPayload( (StringElem)`<HexEscape e>` ) = stringChars( [ hex2int( substring( "<e>", 2 ) ) ] );
private str toPayload( (StringElem)`\\<HexDigit d1><HexDigit d2>` ) = stringChars( [ toInt( "0x<d1><d2>" ) ] );
private default str toPayload( StringElem e ) = "<e>";

private str fromPayloadChar( "\t" ) = "\\t";
private str fromPayloadChar( "\n" ) = "\\n";
private str fromPayloadChar( "\r" ) = "\\r";
private str fromPayloadChar( "\"" ) = "\\\"";
private str fromPayloadChar( "\'" ) = "\\\'";
private str fromPayloadChar( "\\" ) = "\\\\";
private default str fromPayloadChar( str s ) = s;

private list[byte] unicodeCharToUTF8( int unicode ) {
  // Right binary shift is not implemented
  // ">> 6" == "/64"
  // "& 0x1F" == "% 0x20" etc.
  if ( unicode <= 0x7F ) {
    return [ unicode ];
  } else if ( unicode <= 0x7FF ) {
    return [ 0xC0 + ( unicode / 64 ) % 0x20,
             0x80 + unicode % 0x40 ];
  } else if ( unicode <= 0xFFFF ) {
    return [ 0xE0 + ( unicode / ( 64 * 64 ) ) % 0x10,
             0x80 + ( unicode / 64 ) % 0x40,
             0x80 + unicode % 0x40 ];
  } else if ( unicode <= 0x10FFFF ) {
    return [ 0xF0 + ( unicode / ( 64 * 64 * 64 ) ) % 0x08,
             0x80 + ( unicode / ( 2 * 64 ) ) % 0x40,
             0x80 + ( unicode / 64 ) % 0x40,
             0x80 + unicode % 0x40 ];
  } else {
    throw IllegalArgument( unicode );
  }
}

private int utf8Length( (StringElem)`\\t` ) = 1;
private int utf8Length( (StringElem)`\\n` ) = 1;
private int utf8Length( (StringElem)`\\r` ) = 1;
private int utf8Length( (StringElem)`\\"` ) = 1;
private int utf8Length( (StringElem)`\\'` ) = 1;
private int utf8Length( (StringElem)`\\\\` ) = 1;
private int utf8Length( (StringElem)`<HexEscape e>` ) = utf8Length( hex2int( substring( "<e>", 2 ) ) );
private int utf8Length( (StringElem)`\\<HexDigit d1><HexDigit d2>` ) = utf8Length( toInt( "0x<d1><d2>" ) );
private default int utf8Length( StringElem e ) = utf8Length( charAt( "<e>", 0 ) );

private int utf8Length( int unicode ) {
  // Right binary shift is not implemented
  // ">> 6" == "/64"
  // "& 0x1F" == "% 0x20" etc.
  if ( unicode <= 0x7F ) {
    return 1;
  } else if ( unicode <= 0x7FF ) {
    return 2;
  } else if ( unicode <= 0xFFFF ) {
    return 3;
  } else if ( unicode <= 0x10FFFF ) {
    return 4;
  } else {
    throw IllegalArgument( unicode );
  }
}

// WebAssembly hexadecimal integers can contain arbitrary "_" characters
//   strip these before parsing to int
private int hex2int( str s ) = toInt( "0x" + replaceAll( s, "_", "" ) );
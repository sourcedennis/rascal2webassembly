module util::LittleEndian

public alias byte = int;
public alias bytes = list[byte];

public bytes bytesLE( int numBytes, int i ) {
  assert i >= 0 : "i must be positive / <i>";
  return _bytesLE( numBytes, i );
}

public int intLE( bytes b:[ v, *vs ] ) = intLE( vs ) * 0x100 + v;
public int intLE( [] ) = 0;

private bytes _bytesLE( int numBytes:0, int i ) = [];
private bytes _bytesLE( int numBytes, int i ) = [ i % 0x100 ] + _bytesLE( numBytes - 1, i / 0x100 );
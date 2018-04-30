module lang::webassembly::ConvertADT

import IO; // TEMP
import List;
import String;
import lang::webassembly::ADT;
import lang::webassembly::Syntax;

MODULE toADT(start[WebAssembly] src) =
  visit ( src ) {
  case Module m: {
    return toADT( m );
  }
  };
  
MODULE toADT( (Module)`(module <Id? id> <ModuleFields fields>)` ) {
  list[FUNCTYPE] functypes = [];
  list[FUNC] funcs = [];
  list[TABLE] tables = [];
  list[MEM] mems = [];
  list[GLOBAL] globals = [];
  list[ELEM] elems = [];
  list[DATA] \data = [];
  list[START] starts = [];
  list[IMPORT] imports = [];
  list[EXPORT] exports = [];
  
  top-down-break visit( fields ) {
  case FuncType f: println( "FuncType" );
  case Func f: funcs += toADT( f );
  case Table t: println( "Table" );
  case Mem m: println( "Mem" );
  case Global g: println( "Global" );
  case Elem e: println( "Elem" );
  case Data d: println( "Data" );
  case Start s: println( "Start" );
  case Import i: println( "Import" );
  case Export e: println( "Export" );
  }
  
  if ( size( starts ) == 1 ) {
    return \module( functypes, funcs, tables, mems, globals, elems, \data, starts[ 0 ], imports, exports );
  } else {
    return \module( functypes, funcs, tables, mems, globals, elems, \data, imports, exports );
  }
}

VALTYPE toADT( (ValType)`i32` ) = i32( );
VALTYPE toADT( (ValType)`i64` ) = i64( );
VALTYPE toADT( (ValType)`f32` ) = f32( );
VALTYPE toADT( (ValType)`f64` ) = f64( );

list[VALTYPE] toADT( (Locals)`(local <Id _> <ValType valType>) <Locals locals>` ) = toADT( valType ) + toADT( locals );
list[VALTYPE] toADT( (Locals)`(local <ValType valType>) <Locals locals>` ) = toADT( valType ) + toADT( locals );
list[VALTYPE] toADT( (Locals)`` ) = [];

list[INSTR] toADT( (Instrs)`<Instr instr> <Instrs instrs>` ) = toADT( instr ) + toADT( instrs );
list[INSTR] toADT( (Instrs)`` ) = [];

int parseOffset( (Offset?)`offset` ) = 0;
int parseOffset( (Offset?)`` ) = 0;
int parseAlign( (Align)`<Align align>`, int naturalAlignment ) = 0;
int parseAlign( (Align)``, int naturalAlignment ) = naturalAlignment;
tuple[int,int] parseMemArg( (MemArg)`<Offset offset> <Align align>`, int naturalAlignment ) = < parseOffset( offset ), parseAlign( align, naturalAlignment ) >;

INSTR toADT( (Instr)`i32.load8_u <MemArg memArg>` ) = i32_load8_u( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( (Instr)`get_local <LocalIdx idx>` ) = get_local( 0 );
default INSTR toADT( Instr instr ) {
  println( instr );
  return nop( );
}

// Note that after desugaring this is the only syntax option that remains
FUNC toADT( (Func)`(func <Id? _> (type <TypeIdx idx>) <Params ps> <Results rs> <Locals locals> <Instrs instrs>)` ) {
  return func( toInt( "<idx>" ), toADT( locals ), expr(toADT(instrs)) );
}
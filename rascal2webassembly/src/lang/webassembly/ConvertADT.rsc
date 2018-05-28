module lang::webassembly::ConvertADT

import IO; // TEMP
import List;
import String;
import ParseTree;

import lang::webassembly::ADT;
import lang::webassembly::Syntax;
import lang::webassembly::String2UTF8;
import lang::webassembly::execution::Numerics; // for invSigned( )
import lang::webassembly::ToFloat;
import lang::webassembly::ToInt;

MODULE toADT( (start[WebAssembly])`<Module m>` ) = toADT( m );

MODULE toADT( Module m:(Module)`(module <Id? _> <ModuleField* fields>)` ) {
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
  
  IdContext ctx = setupGlobalContext( m );
  
  // This for-loop is necessary, as for big modules a visit on 'm'
  // causes a StackOverflow.
  for ( ModuleField f <- fields ) {
    top-down-break visit( f ) {
	case FuncType f: functypes += toADT( f );
	case Func f: funcs += toADT( ctx, f );
	case Table t: tables += toADT( t );
	case Mem m: mems += toADT( m );
	case Global g: globals += toADT( ctx, g );
	case Elem e: elems += toADT( ctx, e );
	case Data d: \data += toADT( ctx, d );
	case Start s: starts += toADT( ctx, s );
	case Import i: imports += toADT( ctx, i );
	case Export e: exports += toADT( ctx, e );
	}
  }
  
  if ( size( starts ) == 1 ) {
    return \module( functypes, funcs, tables, mems, globals, elems, \data, starts[ 0 ], imports, exports );
  } else {
    return \module( functypes, funcs, tables, mems, globals, elems, \data, imports, exports );
  }
}

FUNCTYPE toADT( (FuncType)`(func <Param* ps> <Result* rs>)` )
  = functype( [ toADT( p ) | p <- ps ], [ toADT( r ) | r <- rs ] );

TABLE toADT( (Table)`(table <Id? _> <TableType t>)`) = table( toADT( t ) );
TABLETYPE toADT( (TableType)`<Limits l> <ElemType et>` ) = tabletype( toADT( l ), toADT( et ) );
LIMITS toADT( (Limits)`<U32 u1>` ) = limits( toIntWasm( "<u1>" ) );
LIMITS toADT( (Limits)`<U32 u1> <U32 u2>` ) = limits( toIntWasm( "<u1>" ), toIntWasm( "<u2>" ) );
ELEMTYPE toADT( ElemType t ) = anyfunc( ); // Only option in WebAssembly 1.0

MEM toADT( (Mem)`(memory <Id? _> <MemType memType>)` ) = mem( toADT( memType ) );
MEMTYPE toADT( (MemType)`<Limits l>` ) = memtype( toADT( l ) );

GLOBAL toADT( IdContext ctx, (Global)`(global <Id? _> <GlobalType gType> <Expr initExpr>)` ) = global( toADT( gType ), toADT( ctx, initExpr ) );
GLOBALTYPE toADT( (GlobalType)`<ValType valType>` ) = globaltype( const( ), toADT( valType ) );
GLOBALTYPE toADT( (GlobalType)`(mut <ValType valType>)` ) = globaltype( var( ), toADT( valType ) );
EXPR toADT( IdContext ctx, (Expr)`<Instr* instrs>` ) = expr( [ toADT( ctx, i ) | i <- instrs ] );
ELEM toADT( IdContext ctx, (Elem)`(elem <TableIdx idx> (offset <Expr expr>) <FuncIdx* idxs>)` ) = elem( getIndex( ctx, idx ), toADT( ctx, expr ), [ getIndex( ctx, i ) | i <- idxs ] );

DATA toADT( IdContext ctx, (Data)`(data <MemIdx idx> (offset <Expr offsetExpr>) <DataString init>)` ) = \data( getIndex( ctx, idx ), toADT( ctx, offsetExpr ), toUTF8Bytes( init ) );

START toADT( IdContext ctx, (Start)`(start <FuncIdx idx>)` ) = \start( getIndex( ctx, idx ) );

IMPORT toADT( IdContext ctx, (Import)`(import <Name modName> <Name importName> <ImportDesc desc>)` )
  = \import( toADT( modName ), toADT( importName ), toADT( ctx, desc ) );

str toADT( Name n ) = toPayload( "<n>" );

IMPORTDESC toADT( IdContext ctx, (ImportDesc)`(func <Id? id> (type <TypeIdx idx>) <Param* ps> <Result* rs>)` ) = importdesc_func( getIndex( ctx, idx ) );
IMPORTDESC toADT( IdContext ctx, (ImportDesc)`(table <Id? id> <TableType t>)` ) = importdesc_table( toADT( t ) );
IMPORTDESC toADT( IdContext ctx, (ImportDesc)`(memory <Id? id> <MemType t>)` ) = importdesc_mem( toADT( t ) );
IMPORTDESC toADT( IdContext ctx, (ImportDesc)`(global <Id? id> <GlobalType t>)` ) = importdesc_global( toADT( t ) );

EXPORT toADT( IdContext ctx, (Export)`(export <Name name> <ExportDesc desc>)` ) = export( toADT( name ), toADT( ctx, desc ) );

EXPORTDESC toADT( IdContext ctx, (ExportDesc)`(func <FuncIdx idx>)` ) = exportdesc_func( getIndex( ctx, idx ) );
EXPORTDESC toADT( IdContext ctx, (ExportDesc)`(table <TableIdx idx>)` ) = exportdesc_table( getIndex( ctx, idx ) );
EXPORTDESC toADT( IdContext ctx, (ExportDesc)`(memory <MemIdx idx>)` ) = exportdesc_mem( getIndex( ctx, idx ) );
EXPORTDESC toADT( IdContext ctx, (ExportDesc)`(global <GlobalIdx idx>)` ) = exportdesc_global( getIndex( ctx, idx ) );
// For some reason this "cheat" is necessary, as the syntax pattern matching otherwise fails
default EXPORTDESC toADT( IdContext ctx, ExportDesc desc ) = toADT( ctx, parse( #ExportDesc, "<desc>" ) );

list[byte] toUTF8Bytes( (DataString)`<String* strings>` ) = [ b | s <- strings, b <- toUTF8Bytes( s ) ];
list[byte] toUTF8Bytes( String s ) = wasmStringToUTF8Bytes( "<s>" );

VALTYPE toADT( (ValType)`i32` ) = i32( );
VALTYPE toADT( (ValType)`i64` ) = i64( );
VALTYPE toADT( (ValType)`f32` ) = f32( );
VALTYPE toADT( (ValType)`f64` ) = f64( );

FUNC toADT( ctx:idContext( types, _, _, _, _, _, _, _, _, _ ), (Func)`(func <Id? _> (type <TypeIdx idx>) <Param* ps> <Result* rs> <Local* locals> <Instr* instrs>)` )
  // Note that the 'ctx2' and 'ctx3' contain the parameters as locals, while the 'adtLocals' does not
  = func( getIndex( ctx, idx ), adtLocals, expr( [ toADT(ctx3, i) | i <- instrs ] ) )
  when typeIdx := getIndex( ctx, idx ),
       <ctx2,adtParams> := toADTAsLocals( ctx, ps ),
       <ctx3,adtLocals> := toADT( ctx2, locals );

tuple[IdContext,list[VALTYPE]] toADTAsLocals( idContext( a, b, c, d, e, f, numLocals, map[str,int] localNames, g, h ), Param* params ) {
  list[VALTYPE] valTypes = [];
  for ( p <- params ) {
    switch ( p ) {
    case (Param)`(param <Id id> <ValType valType>)`: {
      valTypes += toADT( valType );
      localNames[ "<id>" ] = numLocals;
    }
    case (Param)`(param <ValType valType>)`: {
      valTypes += toADT( valType );
    }
    }
    numLocals = numLocals + 1;
  }
  return <idContext( a, b, c, d, e, f, numLocals, localNames, g, h ), valTypes>;
}

tuple[IdContext,list[VALTYPE]] toADT( idContext( a, b, c, d, e, f, numLocals, map[str,int] localNames, g, h ), Local* locals ) {
  list[VALTYPE] valTypes = [];
  for ( l <- locals ) {
    switch ( l ) {
    case (Local)`(local <Id id> <ValType valType>)`: {
      valTypes += toADT( valType );
      localNames[ "<id>" ] = numLocals;
    }
    case (Local)`(local <ValType valType>)`: {
      valTypes += toADT( valType );
    }
    }
    numLocals = numLocals + 1;
  }
  return <idContext( a, b, c, d, e, f, numLocals, localNames, g, h ), valTypes>;
}

VALTYPE toADT( (Local)`(local <Id? _> <ValType valType>)` ) = toADT( valType );
RESULTTYPE toADT( (ResultType)`<Result r>` ) = resulttype( [ toADT( r ) ] );
RESULTTYPE toADT( (ResultType)`` ) = resulttype( [ ] );
VALTYPE toADT( (Result)`(result <Id _> <ValType valType>)` ) = toADT( valType );
VALTYPE toADT( (Result)`(result <ValType valType>)` ) = toADT( valType );
VALTYPE toADT( (Param)`(param <Id _> <ValType valType>)` ) = toADT( valType );
VALTYPE toADT( (Param)`(param <ValType valType>)` ) = toADT( valType );

int parseOffset( Offset offset ) = toIntWasm( substring( "<offset>", 7 ) ); // offset=<U32>
int parseAlign( Align align ) = toIntWasm( substring( "<align>", 6 ) ); // align=<U32>

tuple[int,int] parseMemArg( (MemArg)``, int naturalAlignment ) = < 0, naturalAlignment >;
tuple[int,int] parseMemArg( (MemArg)`<Offset offset>`, int naturalAlignment ) = < parseOffset( offset ), naturalAlignment >;
tuple[int,int] parseMemArg( (MemArg)`<Align align>`, int naturalAlignment ) = < 0, parseAlign( align ) >;
tuple[int,int] parseMemArg( (MemArg)`<Offset offset> <Align align>`, int naturalAlignment ) = < parseOffset( offset ), parseAlign( align ) >;

INSTR toADT( IdContext ctx, (Instr)`i32.const <I32 val>` ) = i32_const( invSigned( 32, toIntWasm( "<val>" ) ) );
INSTR toADT( IdContext ctx, (Instr)`i64.const <I64 val>` ) = i64_const( invSigned( 64, toIntWasm( "<val>" ) ) );
INSTR toADT( IdContext ctx, (Instr)`f32.const <F32 val>` ) = f32_const( toFloat( "<val>" ) );
INSTR toADT( IdContext ctx, (Instr)`f64.const <F64 val>` ) = f64_const( toFloat( "<val>" ) );
// iunop
INSTR toADT( IdContext ctx, (Instr)`i32.clz` ) = i32_clz( );
INSTR toADT( IdContext ctx, (Instr)`i32.ctz` ) = i32_ctz( );
INSTR toADT( IdContext ctx, (Instr)`i32.popcnt` ) = i32_popcnt( );
INSTR toADT( IdContext ctx, (Instr)`i64.clz` ) = i64_clz( );
INSTR toADT( IdContext ctx, (Instr)`i64.ctz` ) = i64_ctz( );
INSTR toADT( IdContext ctx, (Instr)`i64.popcnt` ) = i64_popcnt( );
// funop
INSTR toADT( IdContext ctx, (Instr)`f32.abs` ) = f32_abs( );
INSTR toADT( IdContext ctx, (Instr)`f32.neg` ) = f32_neg( );
INSTR toADT( IdContext ctx, (Instr)`f32.sqrt` ) = f32_sqrt( );
INSTR toADT( IdContext ctx, (Instr)`f32.ceil` ) = f32_ceil( );
INSTR toADT( IdContext ctx, (Instr)`f32.floor` ) = f32_floor( );
INSTR toADT( IdContext ctx, (Instr)`f32.trunc` ) = f32_trunc( );
INSTR toADT( IdContext ctx, (Instr)`f32.nearest` ) = f32_nearest( );
INSTR toADT( IdContext ctx, (Instr)`f64.abs` ) = f64_abs( );
INSTR toADT( IdContext ctx, (Instr)`f64.neg` ) = f64_neg( );
INSTR toADT( IdContext ctx, (Instr)`f64.sqrt` ) = f64_sqrt( );
INSTR toADT( IdContext ctx, (Instr)`f64.ceil` ) = f64_ceil( );
INSTR toADT( IdContext ctx, (Instr)`f64.floor` ) = f64_floor( );
INSTR toADT( IdContext ctx, (Instr)`f64.trunc` ) = f64_trunc( );
INSTR toADT( IdContext ctx, (Instr)`f64.nearest` ) = f64_nearest( );
// ibinop
INSTR toADT( IdContext ctx, (Instr)`i32.add` ) = i32_add( );
INSTR toADT( IdContext ctx, (Instr)`i32.sub` ) = i32_sub( );
INSTR toADT( IdContext ctx, (Instr)`i32.mul` ) = i32_mul( );
INSTR toADT( IdContext ctx, (Instr)`i32.div_u` ) = i32_div_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.div_s` ) = i32_div_s( );
INSTR toADT( IdContext ctx, (Instr)`i32.rem_u` ) = i32_rem_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.rem_s` ) = i32_rem_s( );
INSTR toADT( IdContext ctx, (Instr)`i32.and` ) = i32_and( );
INSTR toADT( IdContext ctx, (Instr)`i32.or` ) = i32_or( );
INSTR toADT( IdContext ctx, (Instr)`i32.xor` ) = i32_xor( );
INSTR toADT( IdContext ctx, (Instr)`i32.shl` ) = i32_shl( );
INSTR toADT( IdContext ctx, (Instr)`i32.shr_u` ) = i32_shr_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.shr_s` ) = i32_shr_s( );
INSTR toADT( IdContext ctx, (Instr)`i32.rotl` ) = i32_rotl( );
INSTR toADT( IdContext ctx, (Instr)`i32.rotr` ) = i32_rotr( );
INSTR toADT( IdContext ctx, (Instr)`i64.add` ) = i64_add( );
INSTR toADT( IdContext ctx, (Instr)`i64.sub` ) = i64_sub( );
INSTR toADT( IdContext ctx, (Instr)`i64.mul` ) = i64_mul( );
INSTR toADT( IdContext ctx, (Instr)`i64.div_u` ) = i64_div_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.div_s` ) = i64_div_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.rem_u` ) = i64_rem_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.rem_s` ) = i64_rem_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.and` ) = i64_and( );
INSTR toADT( IdContext ctx, (Instr)`i64.or` ) = i64_or( );
INSTR toADT( IdContext ctx, (Instr)`i64.xor` ) = i64_xor( );
INSTR toADT( IdContext ctx, (Instr)`i64.shl` ) = i64_shl( );
INSTR toADT( IdContext ctx, (Instr)`i64.shr_u` ) = i64_shr_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.shr_s` ) = i64_shr_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.rotl` ) = i64_rotl( );
INSTR toADT( IdContext ctx, (Instr)`i64.rotr` ) = i64_rotr( );
// fbinop
INSTR toADT( IdContext ctx, (Instr)`f32.add` ) = f32_add( );
INSTR toADT( IdContext ctx, (Instr)`f32.sub` ) = f32_sub( );
INSTR toADT( IdContext ctx, (Instr)`f32.mul` ) = f32_mul( );
INSTR toADT( IdContext ctx, (Instr)`f32.div` ) = f32_div( );
INSTR toADT( IdContext ctx, (Instr)`f32.min` ) = f32_min( );
INSTR toADT( IdContext ctx, (Instr)`f32.max` ) = f32_max( );
INSTR toADT( IdContext ctx, (Instr)`f32.copysign` ) = f32_copysign( );
INSTR toADT( IdContext ctx, (Instr)`f64.add` ) = f64_add( );
INSTR toADT( IdContext ctx, (Instr)`f64.sub` ) = f64_sub( );
INSTR toADT( IdContext ctx, (Instr)`f64.mul` ) = f64_mul( );
INSTR toADT( IdContext ctx, (Instr)`f64.div` ) = f64_div( );
INSTR toADT( IdContext ctx, (Instr)`f64.min` ) = f64_min( );
INSTR toADT( IdContext ctx, (Instr)`f64.max` ) = f64_max( );
INSTR toADT( IdContext ctx, (Instr)`f64.copysign` ) = f64_copysign( );
// itestop
INSTR toADT( IdContext ctx, (Instr)`i32.eqz` ) = i32_eqz( );
INSTR toADT( IdContext ctx, (Instr)`i64.eqz` ) = i64_eqz( );
// irelop
INSTR toADT( IdContext ctx, (Instr)`i32.eq` ) = i32_eq( );
INSTR toADT( IdContext ctx, (Instr)`i32.ne` ) = i32_ne( );
INSTR toADT( IdContext ctx, (Instr)`i32.lt_u` ) = i32_lt_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.lt_s` ) = i32_lt_s( );
INSTR toADT( IdContext ctx, (Instr)`i32.gt_u` ) = i32_gt_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.gt_s` ) = i32_gt_s( );
INSTR toADT( IdContext ctx, (Instr)`i32.le_u` ) = i32_le_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.le_s` ) = i32_le_s( );
INSTR toADT( IdContext ctx, (Instr)`i32.ge_u` ) = i32_ge_u( );
INSTR toADT( IdContext ctx, (Instr)`i32.ge_s` ) = i32_ge_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.eq` ) = i64_eq( );
INSTR toADT( IdContext ctx, (Instr)`i64.ne` ) = i64_ne( );
INSTR toADT( IdContext ctx, (Instr)`i64.lt_u` ) = i64_lt_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.lt_s` ) = i64_lt_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.gt_u` ) = i64_gt_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.gt_s` ) = i64_gt_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.le_u` ) = i64_le_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.le_s` ) = i64_le_s( );
INSTR toADT( IdContext ctx, (Instr)`i64.ge_u` ) = i64_ge_u( );
INSTR toADT( IdContext ctx, (Instr)`i64.ge_s` ) = i64_ge_s( );
// frelop
INSTR toADT( IdContext ctx, (Instr)`f32.eq` ) = f32_eq( );
INSTR toADT( IdContext ctx, (Instr)`f32.ne` ) = f32_ne( );
INSTR toADT( IdContext ctx, (Instr)`f32.lt` ) = f32_lt( );
INSTR toADT( IdContext ctx, (Instr)`f32.gt` ) = f32_gt( );
INSTR toADT( IdContext ctx, (Instr)`f32.le` ) = f32_le( );
INSTR toADT( IdContext ctx, (Instr)`f32.ge` ) = f32_ge( );
INSTR toADT( IdContext ctx, (Instr)`f64.eq` ) = f64_eq( );
INSTR toADT( IdContext ctx, (Instr)`f64.ne` ) = f64_ne( );
INSTR toADT( IdContext ctx, (Instr)`f64.lt` ) = f64_lt( );
INSTR toADT( IdContext ctx, (Instr)`f64.gt` ) = f64_gt( );
INSTR toADT( IdContext ctx, (Instr)`f64.le` ) = f64_le( );
INSTR toADT( IdContext ctx, (Instr)`f64.ge` ) = f64_ge( );
//
INSTR toADT( IdContext ctx, (Instr)`i32.wrap/i64` ) = i32_wrap_i64( );
INSTR toADT( IdContext ctx, (Instr)`i64.extend_u/i32` ) = i64_extend_u_i32( );
INSTR toADT( IdContext ctx, (Instr)`i64.extend_s/i32` ) = i64_extend_s_i32( );
INSTR toADT( IdContext ctx, (Instr)`i32.trunc_u/f32` ) = i32_trunc_u_f32( );
INSTR toADT( IdContext ctx, (Instr)`i32.trunc_s/f32` ) = i32_trunc_s_f32( );
INSTR toADT( IdContext ctx, (Instr)`i32.trunc_u/f64` ) = i32_trunc_u_f64( );
INSTR toADT( IdContext ctx, (Instr)`i32.trunc_s/f64` ) = i32_trunc_s_f64( );
INSTR toADT( IdContext ctx, (Instr)`i64.trunc_u/f32` ) = i64_trunc_u_f32( );
INSTR toADT( IdContext ctx, (Instr)`i64.trunc_s/f32` ) = i64_trunc_s_f32( );
INSTR toADT( IdContext ctx, (Instr)`i64.trunc_u/f64` ) = i64_trunc_u_f64( );
INSTR toADT( IdContext ctx, (Instr)`i64.trunc_s/f64` ) = i64_trunc_s_f64( );
INSTR toADT( IdContext ctx, (Instr)`f32.demote/f64` ) = f32_demote_f64( );
INSTR toADT( IdContext ctx, (Instr)`f64.promote/f32` ) = f64_promote_f32( );
INSTR toADT( IdContext ctx, (Instr)`f32.convert_u/i32` ) = f32_convert_u_i32( );
INSTR toADT( IdContext ctx, (Instr)`f32.convert_s/i32` ) = f32_convert_s_i32( );
INSTR toADT( IdContext ctx, (Instr)`f32.convert_u/i64` ) = f32_convert_u_i64( );
INSTR toADT( IdContext ctx, (Instr)`f32.convert_s/i64` ) = f32_convert_s_i64( );
INSTR toADT( IdContext ctx, (Instr)`f64.convert_u/i32` ) = f64_convert_u_i32( );
INSTR toADT( IdContext ctx, (Instr)`f64.convert_s/i32` ) = f64_convert_s_i32( );
INSTR toADT( IdContext ctx, (Instr)`f64.convert_u/i64` ) = f64_convert_u_i64( );
INSTR toADT( IdContext ctx, (Instr)`f64.convert_s/i64` ) = f64_convert_s_i64( );
INSTR toADT( IdContext ctx, (Instr)`i32.reinterpret/f32` ) = i32_reinterpret_f32( );
INSTR toADT( IdContext ctx, (Instr)`i64.reinterpret/f64` ) = i64_reinterpret_f64( );
INSTR toADT( IdContext ctx, (Instr)`f32.reinterpret/i32` ) = f32_reinterpret_i32( );
INSTR toADT( IdContext ctx, (Instr)`f64.reinterpret/i64` ) = f64_reinterpret_i64( );
// parametric instructions
INSTR toADT( IdContext ctx, (Instr)`drop` ) = drop( );
INSTR toADT( IdContext ctx, (Instr)`select` ) = select( );
// variable instructions
INSTR toADT( IdContext ctx, (Instr)`get_local <LocalIdx idx>` ) = get_local( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`set_local <LocalIdx idx>` ) = set_local( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`tee_local <LocalIdx idx>` ) = tee_local( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`get_global <GlobalIdx idx>` ) = get_global( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`set_global <GlobalIdx idx>` ) = set_global( getIndex( ctx, idx ) );
// memory instructions
// TODO: Check potential logarithmic base of natural alignment
INSTR toADT( IdContext ctx, (Instr)`i32.load <MemArg memArg>` ) = i32_load( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`i64.load <MemArg memArg>` ) = i64_load( offset, align ) when <offset,align> := parseMemArg( memArg, 8 );
INSTR toADT( IdContext ctx, (Instr)`f32.load <MemArg memArg>` ) = f32_load( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`f64.load <MemArg memArg>` ) = f64_load( offset, align ) when <offset,align> := parseMemArg( memArg, 8 );
INSTR toADT( IdContext ctx, (Instr)`i32.store <MemArg memArg>` ) = i32_store( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`i64.store <MemArg memArg>` ) = i64_store( offset, align ) when <offset,align> := parseMemArg( memArg, 8 );
INSTR toADT( IdContext ctx, (Instr)`f32.store <MemArg memArg>` ) = f32_store( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`f64.store <MemArg memArg>` ) = f64_store( offset, align ) when <offset,align> := parseMemArg( memArg, 8 );
INSTR toADT( IdContext ctx, (Instr)`i32.load8_u <MemArg memArg>` ) = i32_load8_u( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i32.load8_s <MemArg memArg>` ) = i32_load8_s( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i64.load8_u <MemArg memArg>` ) = i64_load8_u( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i64.load8_s <MemArg memArg>` ) = i64_load8_s( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i32.load16_u <MemArg memArg>` ) = i32_load16_u( offset, align ) when <offset,align> := parseMemArg( memArg, 2 );
INSTR toADT( IdContext ctx, (Instr)`i32.load16_s <MemArg memArg>` ) = i32_load16_s( offset, align ) when <offset,align> := parseMemArg( memArg, 2 );
INSTR toADT( IdContext ctx, (Instr)`i64.load16_u <MemArg memArg>` ) = i64_load16_u( offset, align ) when <offset,align> := parseMemArg( memArg, 2 );
INSTR toADT( IdContext ctx, (Instr)`i64.load16_s <MemArg memArg>` ) = i64_load16_s( offset, align ) when <offset,align> := parseMemArg( memArg, 2 );
INSTR toADT( IdContext ctx, (Instr)`i64.load32_u <MemArg memArg>` ) = i64_load32_u( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`i64.load32_s <MemArg memArg>` ) = i64_load32_s( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`i32.store8 <MemArg memArg>` ) = i32_store8( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i64.store8 <MemArg memArg>` ) = i64_store8( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i32.store16 <MemArg memArg>` ) = i32_store16( offset, align ) when <offset,align> := parseMemArg( memArg, 2 );
INSTR toADT( IdContext ctx, (Instr)`i64.store16 <MemArg memArg>` ) = i64_store16( offset, align ) when <offset,align> := parseMemArg( memArg, 2 );
INSTR toADT( IdContext ctx, (Instr)`i64.store32 <MemArg memArg>` ) = i64_store32( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
INSTR toADT( IdContext ctx, (Instr)`memory.size` ) = memory_size( );
INSTR toADT( IdContext ctx, (Instr)`memory.grow` ) = memory_grow( );

// control instructions
INSTR toADT( IdContext ctx, (Instr)`nop` ) = nop( );
INSTR toADT( IdContext ctx, (Instr)`unreachable` ) = unreachable( );
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`block <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return block( toADT( resType ), [ toADT( ctx2, i ) | i <- instrs ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`block <Label label> <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return block( toADT( resType ), [ toADT( ctx2, i ) | i <- instrs ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`loop <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return loop( toADT( resType ), [ toADT( ctx2, i ) | i <- instrs ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`loop <Label label> <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return loop( toADT( resType ), [ toADT( ctx2, i ) | i <- instrs ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`if <ResultType resType> <Instr* ifInstrs> else <Id? _> <Instr* elseInstrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toADT( resType ), [ toADT( ctx2, i ) | i <- ifInstrs ], [ toADT( ctx2, i ) | i <- elseInstrs ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`if <Label label> <ResultType resType> <Instr* ifInstrs> else <Id? _> <Instr* elseInstrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toADT( resType ), [ toADT( ctx2, i ) | i <- ifInstrs ], [ toADT( ctx2, i ) | i <- elseInstrs ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`if <ResultType resType> <Instr* ifInstrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toADT( resType ), [ toADT( ctx2, i ) | i <- ifInstrs ], [ ] );
}
INSTR toADT( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`if <Label label> <ResultType resType> <Instr* ifInstrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toADT( resType ), [ toADT( ctx2, i ) | i <- ifInstrs ], [ ] );
}

INSTR toADT( IdContext ctx, (Instr)`br <LabelIdx idx>` ) = br( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`br_if <LabelIdx idx>` ) = br_if( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`br_table <LabelIdx* tableIdxs> <LabelIdx idx>` ) = br_table( [ getIndex( ctx, tIdx ) | tIdx <- tableIdxs ], getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`return` ) = \return( );
INSTR toADT( IdContext ctx, (Instr)`call <FuncIdx idx>` ) = call( getIndex( ctx, idx ) );
INSTR toADT( IdContext ctx, (Instr)`call_indirect (type <TypeIdx idx>) <Param* _> <Result* _>` ) = call_indirect( getIndex( ctx, idx ) );

INSTR toADT( IdContext ctx, (Instr)`i32.load8_u <MemArg memArg>` ) = i32_load8_u( offset, align ) when <offset,align> := parseMemArg( memArg, 1 );
INSTR toADT( IdContext ctx, (Instr)`i32.load <MemArg memArg>` ) = i32_load( offset, align ) when <offset,align> := parseMemArg( memArg, 4 );
default INSTR toADT( IdContext ctx, Instr instr ) = nop( );

// Id Context. It is passed along to most functions, such that names (textual identifiers) can be resolved
data ParamDesc = param( VALTYPE valType, str name ) | param( VALTYPE valType );
data ResultDesc = result( VALTYPE valType, str name ) | result( VALTYPE valType );
alias FuncTypeDesc = tuple[list[ParamDesc],list[ResultDesc]];

data IdContext = idContext( list[FuncTypeDesc] types,
                            map[str,int] typeNames,
                            map[str,int] funcNames,
                            map[str,int] tableNames,
                            map[str,int] memNames,
                            map[str,int] globalNames,
                            // These locals include the parameters
                            //   Note however, that the final ADT does not
                            int numLocals,
                            map[str,int] localNames,
                            int numLabels,
                            map[str,int] labelNames );

ParamDesc toParamDesc( (Param)`(param <Id id> <ValType valType>)` ) = param( toADT( valType ), "<id>" );
ParamDesc toParamDesc( (Param)`(param <ValType valType>)` ) = param( toADT( valType ) );
ResultDesc toResultDesc( (Result)`(result <Id id> <ValType valType>)` ) = result( toADT( valType ), "<id>" );
ResultDesc toResultDesc( (Result)`(result <ValType valType>)` ) = result( toADT( valType ) );

IdContext setupGlobalContext( Module m ) {
  list[FuncTypeDesc] types = [];
  map[str,int] typeNames = ( );
  map[str,int] funcNames = ( );
  map[str,int] tableNames = ( );
  map[str,int] memNames = ( );
  map[str,int] globalNames = ( );
  map[str,int] localNames = ( );
  map[str,int] labelNames = ( );
  
  int numTypes = 0, numFuncs = 0, numTables = 0,
      numMems = 0, numGlobals = 0;
  
  /**
   * Every import defines an index in the respective index space. In each index space,
   * the indices of imports go before the first index of any definition contained in the module itself.
   */
  top-down-break visit( m ) {
  // Func
  case (Import)`(import <Name _> <Name _> (func <Id? id> <TypeUse _>))`: {
    if ( "<id>" != "" ) {
      funcNames[ "<id>" ] = numFuncs;
    }
    numFuncs = numFuncs + 1;
  }
  // Table
  case (Import)`(import <Name _> <Name _> (table <Id? id> <TableType _>))`: {
    if ( "<id>" != "" ) {
      tableNames[ "<id>" ] = numTables;
    }
    numTables = numTables + 1;
  }
  // Mem
  case (Import)`(import <Name _> <Name _> (memory <Id? id> <MemType _>))`: {
    if ( "<id>" != "" ) {
      memNames[ "<id>" ] = numMems;
    }
    numMems = numMems + 1;
  }
  // Global
  case (Import)`(import <Name _> <Name _> (global <Id? id> <GlobalType _>))`: {
    if ( "<id>" != "" ) {
      globalNames[ "<id>" ] = numGlobals;
    }
    numGlobals = numGlobals + 1;
  }
  }
  
  top-down-break visit( m ) {
  // Type
  case (Type)`(type <Id? id> (func <Param* ps> <Result* rs>))`: {
    if ( "<id>" != "" ) {
      typeNames[ "<id>" ] = numTypes;
    }
    types += < [ toParamDesc( p ) | p <- ps ], [ toResultDesc( r ) | r <- rs ] >;
    numTypes = numTypes + 1;
  }
  // Func
  case (Func)`(func <Id? id> <FuncFields _>)`: {
    if ( "<id>" != "" ) {
      funcNames[ "<id>" ] = numFuncs;
    }
    numFuncs = numFuncs + 1;
  }
  // Table
  case (Table)`(table <Id? id> <TableFields _>)`: {
    if ( "<id>" != "" ) {
      tableNames[ "<id>" ] = numTables;
    }
    numTables = numTables + 1;
  }
  // Mem
  case (Mem)`(memory <Id? id> <MemFields _>)`: {
    if ( "<id>" != "" ) {
      memNames[ "<id>" ] = numMems;
    }
    numMems = numMems + 1;
  }
  // Global
  case (Global)`(global <Id? id> <GlobalFields _>)`: {
    if ( "<id>" != "" ) {
      globalNames[ "<id>" ] = numGlobals;
    }
    numGlobals = numGlobals + 1;
  }
  }
  
  return idContext( types, typeNames, funcNames, tableNames, memNames, globalNames, 0, localNames, 0, labelNames ); 
}

// idContext( types, typeNames, funcNames, tableNames, memNames, globalNames, 0, localNames, 0, labelNames )
int getIndex( idContext( _, _, _, _, _, _, _, localNames, _, _ ), (LocalIdx)`<U32 id>` ) = toIntWasm( "<id>" );
int getIndex( idContext( _, _, _, _, _, _, _, localNames, _, _ ), (LocalIdx)`<Id id>` ) = localNames[ "<id>" ];
int getIndex( idContext( _, typeNames, _, _, _, _, _, _, _, _ ), (TypeIdx)`<U32 id>` ) = toIntWasm( "<id>" );
int getIndex( idContext( _, typeNames, _, _, _, _, _, _, _, _ ), (TypeIdx)`<Id id>` ) = typeNames[ "<id>" ];
int getIndex( idContext( _, _, _, _, _, globalNames, _, _, _, _ ), (GlobalIdx)`<U32 id>` ) = toIntWasm( "<id>" );
int getIndex( idContext( _, _, _, _, _, globalNames, _, _, _, _ ), (GlobalIdx)`<Id id>` ) = globalNames[ "<id>" ];
int getIndex( idContext( _, _, _, _, _, _, _, _, numLabels, labelNames ), (LabelIdx)`<U32 id>` ) = toIntWasm( "<id>" );
// Label 0 refers to the innermost control structure encapsulating the instruction referencing it
int getIndex( idContext( _, _, _, _, _, _, _, _, numLabels, labelNames ), (LabelIdx)`<Id id>` ) = numLabels - 1 - labelNames[ "<id>" ];
int getIndex( idContext( _, _, funcNames, _, _, _, _, _, _, _ ), (FuncIdx)`<U32 id>` ) = toIntWasm( "<id>" );
int getIndex( idContext( _, _, funcNames, _, _, _, _, _, _, _ ), (FuncIdx)`<Id id>` ) = funcNames[ "<id>" ];
int getIndex( idContext( _, _, _, _, memNames, _, _, _, _, _ ), (MemIdx)`<U32 id>` ) = toIntWasm( "<id>" );
int getIndex( idContext( _, _, _, _, memNames, _, _, _, _, _ ), (MemIdx)`<Id id>` ) = memNames[ "<id>" ];
int getIndex( idContext( _, _, _, tableNames, _, _, _, _, _, _ ), (TableIdx)`<U32 id>` ) = toIntWasm( "<id>" );
int getIndex( idContext( _, _, _, tableNames, _, _, _, _, _, _ ), (TableIdx)`<Id id>` ) = tableNames[ "<id>" ];

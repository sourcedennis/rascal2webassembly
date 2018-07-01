module lang::webassembly::Syntax2AST

//
// Converts a WebAssembly 1.0 module from its parse tree representation into
// its AST.
//
// Public functions:
//  - MODULE toAST( start[WebAssembly] )
//  - MODULE toAST( Module )
//
// Important: The parse tree MUST be desugared before applying these operations.
//

import List;
import String;
import ParseTree;

import lang::webassembly::Abstract;
import lang::webassembly::Syntax;
import lang::webassembly::util::String2UTF8;
import lang::webassembly::execution::Numerics; // for invSigned( )
import lang::webassembly::util::ToFloat;
import lang::webassembly::util::ToInt;

public MODULE toAST( (start[WebAssembly])`<Module m>` ) = toAST( m );

public MODULE toAST( Module m:(Module)`(module <Id? _> <ModuleField* fields>)` ) {
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
    // In some rare cases pattern matching fails.
    // After applying this cheat, it does not. Not sure why
	f = parse( #ModuleField, "<f>" );
	  
    top-down-break visit( f ) {
	case FuncType f: functypes += toAST( f );
	case Func f: funcs += toAST( ctx, f );
	case Table t: tables += toAST( t );
	case Mem m: mems += toAST( m );
	case Global g: globals += toAST( ctx, g );
	case Elem e: elems += toAST( ctx, e );
	case Data d: \data += toAST( ctx, d );
	case Start s: starts += toAST( ctx, s );
	case Import i: imports += toAST( ctx, i );
	case Export e: exports += toAST( ctx, e );
	}
  }
  
  if ( size( starts ) == 1 ) {
    return \module( functypes, funcs, tables, mems, globals, elems, \data, starts[ 0 ], imports, exports );
  } else {
    return \module( functypes, funcs, tables, mems, globals, elems, \data, imports, exports );
  }
}

private FUNCTYPE toAST( (FuncType)`(func <Param* ps> <Result* rs>)` )
  = functype( [ toAST( p ) | p <- ps ], [ toAST( r ) | r <- rs ] );

private TABLE toAST( (Table)`(table <Id? _> <TableType t>)`) = table( toAST( t ) );
private TABLETYPE toAST( (TableType)`<Limits l> <ElemType et>` ) = tabletype( toAST( l ), toAST( et ) );
private LIMITS toAST( (Limits)`<U32 u1>` ) = limits( toIntWasm( "<u1>" ) );
private LIMITS toAST( (Limits)`<U32 u1> <U32 u2>` ) = limits( toIntWasm( "<u1>" ), toIntWasm( "<u2>" ) );
private ELEMTYPE toAST( ElemType t ) = anyfunc( ); // Only option in WebAssembly 1.0

private MEM toAST( (Mem)`(memory <Id? _> <MemType memType>)` ) = mem( toAST( memType ) );
private MEMTYPE toAST( (MemType)`<Limits l>` ) = memtype( toAST( l ) );

private GLOBAL toAST( IdContext ctx, (Global)`(global <Id? _> <GlobalType gType> <Expr initExpr>)` ) = global( toAST( gType ), toAST( ctx, initExpr ) );
private GLOBALTYPE toAST( (GlobalType)`<ValType valType>` ) = globaltype( const( ), toAST( valType ) );
private GLOBALTYPE toAST( (GlobalType)`(mut <ValType valType>)` ) = globaltype( var( ), toAST( valType ) );
private EXPR toAST( IdContext ctx, (Expr)`<Instr* instrs>` ) = expr( [ toAST( ctx, i ) | i <- instrs ] );
private ELEM toAST( IdContext ctx, (Elem)`(elem <TableIdx idx> (offset <Expr expr>) <FuncIdx* idxs>)` ) = elem( getIndex( ctx, idx ), toAST( ctx, expr ), [ getIndex( ctx, i ) | i <- idxs ] );

private DATA toAST( IdContext ctx, (Data)`(data <MemIdx idx> (offset <Expr offsetExpr>) <DataString init>)` ) = \data( getIndex( ctx, idx ), toAST( ctx, offsetExpr ), toUTF8Bytes( init ) );

private START toAST( IdContext ctx, (Start)`(start <FuncIdx idx>)` ) = \start( getIndex( ctx, idx ) );

private IMPORT toAST( IdContext ctx, (Import)`(import <Name modName> <Name importName> <ImportDesc desc>)` )
  = \import( toAST( modName ), toAST( importName ), toAST( ctx, desc ) );

private str toAST( Name n ) = toPayload( "<n>" );

private IMPORTDESC toAST( IdContext ctx, (ImportDesc)`(func <Id? id> (type <TypeIdx idx>) <Param* ps> <Result* rs>)` ) = importdesc_func( getIndex( ctx, idx ) );
private IMPORTDESC toAST( IdContext ctx, (ImportDesc)`(table <Id? id> <TableType t>)` ) = importdesc_table( toAST( t ) );
private IMPORTDESC toAST( IdContext ctx, (ImportDesc)`(memory <Id? id> <MemType t>)` ) = importdesc_mem( toAST( t ) );
private IMPORTDESC toAST( IdContext ctx, (ImportDesc)`(global <Id? id> <GlobalType t>)` ) = importdesc_global( toAST( t ) );

private EXPORT toAST( IdContext ctx, (Export)`(export <Name name> <ExportDesc desc>)` ) = export( toAST( name ), toAST( ctx, desc ) );

private EXPORTDESC toAST( IdContext ctx, (ExportDesc)`(func <FuncIdx idx>)` ) = exportdesc_func( getIndex( ctx, idx ) );
private EXPORTDESC toAST( IdContext ctx, (ExportDesc)`(table <TableIdx idx>)` ) = exportdesc_table( getIndex( ctx, idx ) );
private EXPORTDESC toAST( IdContext ctx, (ExportDesc)`(memory <MemIdx idx>)` ) = exportdesc_mem( getIndex( ctx, idx ) );
private EXPORTDESC toAST( IdContext ctx, (ExportDesc)`(global <GlobalIdx idx>)` ) = exportdesc_global( getIndex( ctx, idx ) );
// For some reason this "cheat" is necessary, as the syntax pattern matching otherwise fails
private default EXPORTDESC toAST( IdContext ctx, ExportDesc desc ) = toAST( ctx, parse( #ExportDesc, "<desc>" ) );

private list[byte] toUTF8Bytes( (DataString)`<String* strings>` ) = [ b | s <- strings, b <- toUTF8Bytes( s ) ];
private list[byte] toUTF8Bytes( String s ) = wasmStringToUTF8Bytes( "<s>" );

private VALTYPE toAST( (ValType)`i32` ) = i32( );
private VALTYPE toAST( (ValType)`i64` ) = i64( );
private VALTYPE toAST( (ValType)`f32` ) = f32( );
private VALTYPE toAST( (ValType)`f64` ) = f64( );

private FUNC toAST( ctx:idContext( types, _, _, _, _, _, _, _, _, _ ), (Func)`(func <Id? _> (type <TypeIdx idx>) <Param* ps> <Result* rs> <Local* locals> <Instr* instrs>)` )
  // Note that the 'ctx2' and 'ctx3' contain the parameters as locals, while the 'adtLocals' does not
  = func( getIndex( ctx, idx ), adtLocals, expr( [ toAST(ctx3, i) | i <- instrs ] ) )
  when typeIdx := getIndex( ctx, idx ),
       <ctx2,adtParams> := toASTAsLocals( ctx, ps ),
       <ctx3,adtLocals> := toAST( ctx2, locals );

private tuple[IdContext,list[VALTYPE]] toASTAsLocals( idContext( a, b, c, d, e, f, numLocals, map[str,int] localNames, g, h ), Param* params ) {
  list[VALTYPE] valTypes = [];
  for ( p <- params ) {
    switch ( p ) {
    case (Param)`(param <Id id> <ValType valType>)`: {
      valTypes += toAST( valType );
      localNames[ "<id>" ] = numLocals;
    }
    case (Param)`(param <ValType valType>)`: {
      valTypes += toAST( valType );
    }
    }
    numLocals = numLocals + 1;
  }
  return <idContext( a, b, c, d, e, f, numLocals, localNames, g, h ), valTypes>;
}

private tuple[IdContext,list[VALTYPE]] toAST( idContext( a, b, c, d, e, f, numLocals, map[str,int] localNames, g, h ), Local* locals ) {
  list[VALTYPE] valTypes = [];
  for ( l <- locals ) {
    switch ( l ) {
    case (Local)`(local <Id id> <ValType valType>)`: {
      valTypes += toAST( valType );
      localNames[ "<id>" ] = numLocals;
    }
    case (Local)`(local <ValType valType>)`: {
      valTypes += toAST( valType );
    }
    }
    numLocals = numLocals + 1;
  }
  return <idContext( a, b, c, d, e, f, numLocals, localNames, g, h ), valTypes>;
}

private VALTYPE toAST( (Local)`(local <Id? _> <ValType valType>)` ) = toAST( valType );
private RESULTTYPE toAST( (ResultType)`<Result r>` ) = resulttype( [ toAST( r ) ] );
private RESULTTYPE toAST( (ResultType)`` ) = resulttype( [ ] );
private VALTYPE toAST( (Result)`(result <Id _> <ValType valType>)` ) = toAST( valType );
private VALTYPE toAST( (Result)`(result <ValType valType>)` ) = toAST( valType );
private VALTYPE toAST( (Param)`(param <Id _> <ValType valType>)` ) = toAST( valType );
private VALTYPE toAST( (Param)`(param <ValType valType>)` ) = toAST( valType );

private int parseOffset( Offset offset ) = toIntWasm( substring( "<offset>", 7 ) ); // offset=<U32>
private int parseAlign( Align align ) = toIntWasm( substring( "<align>", 6 ) ); // align=<U32>

private tuple[int,int] parseMemArg( (MemArg)``, int naturalAlignment ) = < 0, naturalAlignment >;
private tuple[int,int] parseMemArg( (MemArg)`<Offset offset>`, int naturalAlignment ) = < parseOffset( offset ), naturalAlignment >;
private tuple[int,int] parseMemArg( (MemArg)`<Align align>`, int naturalAlignment ) = < 0, parseAlign( align ) >;
private tuple[int,int] parseMemArg( (MemArg)`<Offset offset> <Align align>`, int naturalAlignment ) = < parseOffset( offset ), parseAlign( align ) >;

private INSTR toAST( IdContext ctx, (Instr)`i32.const <I32 val>` ) = i32_const( invSigned( 32, toIntWasm( "<val>" ) ) );
private INSTR toAST( IdContext ctx, (Instr)`i64.const <I64 val>` ) = i64_const( invSigned( 64, toIntWasm( "<val>" ) ) );
private INSTR toAST( IdContext ctx, (Instr)`f32.const <F32 val>` ) = f32_const( toFloat( "<val>" ) );
private INSTR toAST( IdContext ctx, (Instr)`f64.const <F64 val>` ) = f64_const( toFloat( "<val>" ) );
// iunop
private INSTR toAST( IdContext ctx, (Instr)`i32.clz` ) = i32_clz( );
private INSTR toAST( IdContext ctx, (Instr)`i32.ctz` ) = i32_ctz( );
private INSTR toAST( IdContext ctx, (Instr)`i32.popcnt` ) = i32_popcnt( );
private INSTR toAST( IdContext ctx, (Instr)`i64.clz` ) = i64_clz( );
private INSTR toAST( IdContext ctx, (Instr)`i64.ctz` ) = i64_ctz( );
private INSTR toAST( IdContext ctx, (Instr)`i64.popcnt` ) = i64_popcnt( );
// funop
private INSTR toAST( IdContext ctx, (Instr)`f32.abs` ) = f32_abs( );
private INSTR toAST( IdContext ctx, (Instr)`f32.neg` ) = f32_neg( );
private INSTR toAST( IdContext ctx, (Instr)`f32.sqrt` ) = f32_sqrt( );
private INSTR toAST( IdContext ctx, (Instr)`f32.ceil` ) = f32_ceil( );
private INSTR toAST( IdContext ctx, (Instr)`f32.floor` ) = f32_floor( );
private INSTR toAST( IdContext ctx, (Instr)`f32.trunc` ) = f32_trunc( );
private INSTR toAST( IdContext ctx, (Instr)`f32.nearest` ) = f32_nearest( );
private INSTR toAST( IdContext ctx, (Instr)`f64.abs` ) = f64_abs( );
private INSTR toAST( IdContext ctx, (Instr)`f64.neg` ) = f64_neg( );
private INSTR toAST( IdContext ctx, (Instr)`f64.sqrt` ) = f64_sqrt( );
private INSTR toAST( IdContext ctx, (Instr)`f64.ceil` ) = f64_ceil( );
private INSTR toAST( IdContext ctx, (Instr)`f64.floor` ) = f64_floor( );
private INSTR toAST( IdContext ctx, (Instr)`f64.trunc` ) = f64_trunc( );
private INSTR toAST( IdContext ctx, (Instr)`f64.nearest` ) = f64_nearest( );
// ibinop
private INSTR toAST( IdContext ctx, (Instr)`i32.add` ) = i32_add( );
private INSTR toAST( IdContext ctx, (Instr)`i32.sub` ) = i32_sub( );
private INSTR toAST( IdContext ctx, (Instr)`i32.mul` ) = i32_mul( );
private INSTR toAST( IdContext ctx, (Instr)`i32.div_u` ) = i32_div_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.div_s` ) = i32_div_s( );
private INSTR toAST( IdContext ctx, (Instr)`i32.rem_u` ) = i32_rem_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.rem_s` ) = i32_rem_s( );
private INSTR toAST( IdContext ctx, (Instr)`i32.and` ) = i32_and( );
private INSTR toAST( IdContext ctx, (Instr)`i32.or` ) = i32_or( );
private INSTR toAST( IdContext ctx, (Instr)`i32.xor` ) = i32_xor( );
private INSTR toAST( IdContext ctx, (Instr)`i32.shl` ) = i32_shl( );
private INSTR toAST( IdContext ctx, (Instr)`i32.shr_u` ) = i32_shr_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.shr_s` ) = i32_shr_s( );
private INSTR toAST( IdContext ctx, (Instr)`i32.rotl` ) = i32_rotl( );
private INSTR toAST( IdContext ctx, (Instr)`i32.rotr` ) = i32_rotr( );
private INSTR toAST( IdContext ctx, (Instr)`i64.add` ) = i64_add( );
private INSTR toAST( IdContext ctx, (Instr)`i64.sub` ) = i64_sub( );
private INSTR toAST( IdContext ctx, (Instr)`i64.mul` ) = i64_mul( );
private INSTR toAST( IdContext ctx, (Instr)`i64.div_u` ) = i64_div_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.div_s` ) = i64_div_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.rem_u` ) = i64_rem_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.rem_s` ) = i64_rem_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.and` ) = i64_and( );
private INSTR toAST( IdContext ctx, (Instr)`i64.or` ) = i64_or( );
private INSTR toAST( IdContext ctx, (Instr)`i64.xor` ) = i64_xor( );
private INSTR toAST( IdContext ctx, (Instr)`i64.shl` ) = i64_shl( );
private INSTR toAST( IdContext ctx, (Instr)`i64.shr_u` ) = i64_shr_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.shr_s` ) = i64_shr_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.rotl` ) = i64_rotl( );
private INSTR toAST( IdContext ctx, (Instr)`i64.rotr` ) = i64_rotr( );
// fbinop
private INSTR toAST( IdContext ctx, (Instr)`f32.add` ) = f32_add( );
private INSTR toAST( IdContext ctx, (Instr)`f32.sub` ) = f32_sub( );
private INSTR toAST( IdContext ctx, (Instr)`f32.mul` ) = f32_mul( );
private INSTR toAST( IdContext ctx, (Instr)`f32.div` ) = f32_div( );
private INSTR toAST( IdContext ctx, (Instr)`f32.min` ) = f32_min( );
private INSTR toAST( IdContext ctx, (Instr)`f32.max` ) = f32_max( );
private INSTR toAST( IdContext ctx, (Instr)`f32.copysign` ) = f32_copysign( );
private INSTR toAST( IdContext ctx, (Instr)`f64.add` ) = f64_add( );
private INSTR toAST( IdContext ctx, (Instr)`f64.sub` ) = f64_sub( );
private INSTR toAST( IdContext ctx, (Instr)`f64.mul` ) = f64_mul( );
private INSTR toAST( IdContext ctx, (Instr)`f64.div` ) = f64_div( );
private INSTR toAST( IdContext ctx, (Instr)`f64.min` ) = f64_min( );
private INSTR toAST( IdContext ctx, (Instr)`f64.max` ) = f64_max( );
private INSTR toAST( IdContext ctx, (Instr)`f64.copysign` ) = f64_copysign( );
// itestop
private INSTR toAST( IdContext ctx, (Instr)`i32.eqz` ) = i32_eqz( );
private INSTR toAST( IdContext ctx, (Instr)`i64.eqz` ) = i64_eqz( );
// irelop
private INSTR toAST( IdContext ctx, (Instr)`i32.eq` ) = i32_eq( );
private INSTR toAST( IdContext ctx, (Instr)`i32.ne` ) = i32_ne( );
private INSTR toAST( IdContext ctx, (Instr)`i32.lt_u` ) = i32_lt_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.lt_s` ) = i32_lt_s( );
private INSTR toAST( IdContext ctx, (Instr)`i32.gt_u` ) = i32_gt_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.gt_s` ) = i32_gt_s( );
private INSTR toAST( IdContext ctx, (Instr)`i32.le_u` ) = i32_le_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.le_s` ) = i32_le_s( );
private INSTR toAST( IdContext ctx, (Instr)`i32.ge_u` ) = i32_ge_u( );
private INSTR toAST( IdContext ctx, (Instr)`i32.ge_s` ) = i32_ge_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.eq` ) = i64_eq( );
private INSTR toAST( IdContext ctx, (Instr)`i64.ne` ) = i64_ne( );
private INSTR toAST( IdContext ctx, (Instr)`i64.lt_u` ) = i64_lt_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.lt_s` ) = i64_lt_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.gt_u` ) = i64_gt_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.gt_s` ) = i64_gt_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.le_u` ) = i64_le_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.le_s` ) = i64_le_s( );
private INSTR toAST( IdContext ctx, (Instr)`i64.ge_u` ) = i64_ge_u( );
private INSTR toAST( IdContext ctx, (Instr)`i64.ge_s` ) = i64_ge_s( );
// frelop
private INSTR toAST( IdContext ctx, (Instr)`f32.eq` ) = f32_eq( );
private INSTR toAST( IdContext ctx, (Instr)`f32.ne` ) = f32_ne( );
private INSTR toAST( IdContext ctx, (Instr)`f32.lt` ) = f32_lt( );
private INSTR toAST( IdContext ctx, (Instr)`f32.gt` ) = f32_gt( );
private INSTR toAST( IdContext ctx, (Instr)`f32.le` ) = f32_le( );
private INSTR toAST( IdContext ctx, (Instr)`f32.ge` ) = f32_ge( );
private INSTR toAST( IdContext ctx, (Instr)`f64.eq` ) = f64_eq( );
private INSTR toAST( IdContext ctx, (Instr)`f64.ne` ) = f64_ne( );
private INSTR toAST( IdContext ctx, (Instr)`f64.lt` ) = f64_lt( );
private INSTR toAST( IdContext ctx, (Instr)`f64.gt` ) = f64_gt( );
private INSTR toAST( IdContext ctx, (Instr)`f64.le` ) = f64_le( );
private INSTR toAST( IdContext ctx, (Instr)`f64.ge` ) = f64_ge( );
//
private INSTR toAST( IdContext ctx, (Instr)`i32.wrap/i64` ) = i32_wrap_i64( );
private INSTR toAST( IdContext ctx, (Instr)`i64.extend_u/i32` ) = i64_extend_u_i32( );
private INSTR toAST( IdContext ctx, (Instr)`i64.extend_s/i32` ) = i64_extend_s_i32( );
private INSTR toAST( IdContext ctx, (Instr)`i32.trunc_u/f32` ) = i32_trunc_u_f32( );
private INSTR toAST( IdContext ctx, (Instr)`i32.trunc_s/f32` ) = i32_trunc_s_f32( );
private INSTR toAST( IdContext ctx, (Instr)`i32.trunc_u/f64` ) = i32_trunc_u_f64( );
private INSTR toAST( IdContext ctx, (Instr)`i32.trunc_s/f64` ) = i32_trunc_s_f64( );
private INSTR toAST( IdContext ctx, (Instr)`i64.trunc_u/f32` ) = i64_trunc_u_f32( );
private INSTR toAST( IdContext ctx, (Instr)`i64.trunc_s/f32` ) = i64_trunc_s_f32( );
private INSTR toAST( IdContext ctx, (Instr)`i64.trunc_u/f64` ) = i64_trunc_u_f64( );
private INSTR toAST( IdContext ctx, (Instr)`i64.trunc_s/f64` ) = i64_trunc_s_f64( );
private INSTR toAST( IdContext ctx, (Instr)`f32.demote/f64` ) = f32_demote_f64( );
private INSTR toAST( IdContext ctx, (Instr)`f64.promote/f32` ) = f64_promote_f32( );
private INSTR toAST( IdContext ctx, (Instr)`f32.convert_u/i32` ) = f32_convert_u_i32( );
private INSTR toAST( IdContext ctx, (Instr)`f32.convert_s/i32` ) = f32_convert_s_i32( );
private INSTR toAST( IdContext ctx, (Instr)`f32.convert_u/i64` ) = f32_convert_u_i64( );
private INSTR toAST( IdContext ctx, (Instr)`f32.convert_s/i64` ) = f32_convert_s_i64( );
private INSTR toAST( IdContext ctx, (Instr)`f64.convert_u/i32` ) = f64_convert_u_i32( );
private INSTR toAST( IdContext ctx, (Instr)`f64.convert_s/i32` ) = f64_convert_s_i32( );
private INSTR toAST( IdContext ctx, (Instr)`f64.convert_u/i64` ) = f64_convert_u_i64( );
private INSTR toAST( IdContext ctx, (Instr)`f64.convert_s/i64` ) = f64_convert_s_i64( );
private INSTR toAST( IdContext ctx, (Instr)`i32.reinterpret/f32` ) = i32_reinterpret_f32( );
private INSTR toAST( IdContext ctx, (Instr)`i64.reinterpret/f64` ) = i64_reinterpret_f64( );
private INSTR toAST( IdContext ctx, (Instr)`f32.reinterpret/i32` ) = f32_reinterpret_i32( );
private INSTR toAST( IdContext ctx, (Instr)`f64.reinterpret/i64` ) = f64_reinterpret_i64( );
// parametric instructions
private INSTR toAST( IdContext ctx, (Instr)`drop` ) = drop( );
private INSTR toAST( IdContext ctx, (Instr)`select` ) = select( );
// variable instructions
private INSTR toAST( IdContext ctx, (Instr)`get_local <LocalIdx idx>` ) = get_local( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`set_local <LocalIdx idx>` ) = set_local( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`tee_local <LocalIdx idx>` ) = tee_local( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`get_global <GlobalIdx idx>` ) = get_global( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`set_global <GlobalIdx idx>` ) = set_global( getIndex( ctx, idx ) );
// memory instructions
private int NATALIGN_8  = 0; // 2^0 = 1 bytes = 8 bits
private int NATALIGN_16 = 1; // 2^1 = 2 bytes = 16 bits
private int NATALIGN_32 = 2; // 2^2 = 4 bytes = 32 bits
private int NATALIGN_64 = 3; // 2^3 = 8 bytes = 64 bits
private INSTR toAST( IdContext ctx, (Instr)`i32.load <MemArg memArg>` ) = i32_load( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load <MemArg memArg>` ) = i64_load( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_64 );
private INSTR toAST( IdContext ctx, (Instr)`f32.load <MemArg memArg>` ) = f32_load( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`f64.load <MemArg memArg>` ) = f64_load( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_64 );
private INSTR toAST( IdContext ctx, (Instr)`i32.store <MemArg memArg>` ) = i32_store( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`i64.store <MemArg memArg>` ) = i64_store( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_64 );
private INSTR toAST( IdContext ctx, (Instr)`f32.store <MemArg memArg>` ) = f32_store( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`f64.store <MemArg memArg>` ) = f64_store( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_64 );
private INSTR toAST( IdContext ctx, (Instr)`i32.load8_u <MemArg memArg>` ) = i32_load8_u( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_8 );
private INSTR toAST( IdContext ctx, (Instr)`i32.load8_s <MemArg memArg>` ) = i32_load8_s( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_8 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load8_u <MemArg memArg>` ) = i64_load8_u( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_8 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load8_s <MemArg memArg>` ) = i64_load8_s( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_8 );
private INSTR toAST( IdContext ctx, (Instr)`i32.load16_u <MemArg memArg>` ) = i32_load16_u( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_16 );
private INSTR toAST( IdContext ctx, (Instr)`i32.load16_s <MemArg memArg>` ) = i32_load16_s( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_16 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load16_u <MemArg memArg>` ) = i64_load16_u( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_16 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load16_s <MemArg memArg>` ) = i64_load16_s( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_16 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load32_u <MemArg memArg>` ) = i64_load32_u( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`i64.load32_s <MemArg memArg>` ) = i64_load32_s( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`i32.store8 <MemArg memArg>` ) = i32_store8( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_8 );
private INSTR toAST( IdContext ctx, (Instr)`i64.store8 <MemArg memArg>` ) = i64_store8( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_8 );
private INSTR toAST( IdContext ctx, (Instr)`i32.store16 <MemArg memArg>` ) = i32_store16( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_16 );
private INSTR toAST( IdContext ctx, (Instr)`i64.store16 <MemArg memArg>` ) = i64_store16( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_16 );
private INSTR toAST( IdContext ctx, (Instr)`i64.store32 <MemArg memArg>` ) = i64_store32( offset, align ) when <offset,align> := parseMemArg( memArg, NATALIGN_32 );
private INSTR toAST( IdContext ctx, (Instr)`memory.size` ) = memory_size( );
private INSTR toAST( IdContext ctx, (Instr)`memory.grow` ) = memory_grow( );

// control instructions
private INSTR toAST( IdContext ctx, (Instr)`nop` ) = nop( );
private INSTR toAST( IdContext ctx, (Instr)`unreachable` ) = unreachable( );
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`block <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return block( toAST( resType ), [ toAST( ctx2, i ) | i <- instrs ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`block <Label label> <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return block( toAST( resType ), [ toAST( ctx2, i ) | i <- instrs ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`loop <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return loop( toAST( resType ), [ toAST( ctx2, i ) | i <- instrs ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`loop <Label label> <ResultType resType> <Instr* instrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return loop( toAST( resType ), [ toAST( ctx2, i ) | i <- instrs ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`if <ResultType resType> <Instr* ifInstrs> else <Id? _> <Instr* elseInstrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toAST( resType ), [ toAST( ctx2, i ) | i <- ifInstrs ], [ toAST( ctx2, i ) | i <- elseInstrs ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`if <Label label> <ResultType resType> <Instr* ifInstrs> else <Id? _> <Instr* elseInstrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toAST( resType ), [ toAST( ctx2, i ) | i <- ifInstrs ], [ toAST( ctx2, i ) | i <- elseInstrs ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, labelNames ), (Instr)`if <ResultType resType> <Instr* ifInstrs> end <Id? _>` ) {
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toAST( resType ), [ toAST( ctx2, i ) | i <- ifInstrs ], [ ] );
}
private INSTR toAST( ctx:idContext( a, b, c, d, e, f, g, h, numLabels, map[str,int] labelNames ), (Instr)`if <Label label> <ResultType resType> <Instr* ifInstrs> end <Id? _>` ) {
  labelNames[ "<label>" ] = numLabels;
  ctx2 = idContext( a, b, c, d, e, f, g, h, numLabels + 1, labelNames );
  return \if( toAST( resType ), [ toAST( ctx2, i ) | i <- ifInstrs ], [ ] );
}

private INSTR toAST( IdContext ctx, (Instr)`br <LabelIdx idx>` ) = br( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`br_if <LabelIdx idx>` ) = br_if( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`br_table <LabelIdx* tableIdxs> <LabelIdx idx>` ) = br_table( [ getIndex( ctx, tIdx ) | tIdx <- tableIdxs ], getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`return` ) = \return( );
private INSTR toAST( IdContext ctx, (Instr)`call <FuncIdx idx>` ) = call( getIndex( ctx, idx ) );
private INSTR toAST( IdContext ctx, (Instr)`call_indirect (type <TypeIdx idx>) <Param* _> <Result* _>` ) = call_indirect( getIndex( ctx, idx ) );

private default INSTR toAST( IdContext ctx, Instr instr ) = nop( );

// Id Context. It is passed along to most functions, such that names (textual identifiers) can be resolved
private data ParamDesc = param( VALTYPE valType, str name ) | param( VALTYPE valType );
private data ResultDesc = result( VALTYPE valType, str name ) | result( VALTYPE valType );
private alias FuncTypeDesc = tuple[list[ParamDesc],list[ResultDesc]];

private data IdContext = idContext( list[FuncTypeDesc] types,
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

private ParamDesc toParamDesc( (Param)`(param <Id id> <ValType valType>)` ) = param( toAST( valType ), "<id>" );
private ParamDesc toParamDesc( (Param)`(param <ValType valType>)` ) = param( toAST( valType ) );
private ResultDesc toResultDesc( (Result)`(result <Id id> <ValType valType>)` ) = result( toAST( valType ), "<id>" );
private ResultDesc toResultDesc( (Result)`(result <ValType valType>)` ) = result( toAST( valType ) );

private IdContext setupGlobalContext( Module m ) {
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
private int getIndex( idContext( _, _, _, _, _, _, _, localNames, _, _ ), (LocalIdx)`<U32 id>` ) = toIntWasm( "<id>" );
private int getIndex( idContext( _, _, _, _, _, _, _, localNames, _, _ ), (LocalIdx)`<Id id>` ) = localNames[ "<id>" ];
private int getIndex( idContext( _, typeNames, _, _, _, _, _, _, _, _ ), (TypeIdx)`<U32 id>` ) = toIntWasm( "<id>" );
private int getIndex( idContext( _, typeNames, _, _, _, _, _, _, _, _ ), (TypeIdx)`<Id id>` ) = typeNames[ "<id>" ];
private int getIndex( idContext( _, _, _, _, _, globalNames, _, _, _, _ ), (GlobalIdx)`<U32 id>` ) = toIntWasm( "<id>" );
private int getIndex( idContext( _, _, _, _, _, globalNames, _, _, _, _ ), (GlobalIdx)`<Id id>` ) = globalNames[ "<id>" ];
private int getIndex( idContext( _, _, _, _, _, _, _, _, int numLabels, map[str,int] labelNames ), (LabelIdx)`<U32 id>` ) = toIntWasm( "<id>" );
// Label 0 refers to the innermost control structure encapsulating the instruction referencing it
private int getIndex( idContext( _, _, _, _, _, _, _, _, int numLabels, map[str,int] labelNames ), (LabelIdx)`<Id id>` ) = numLabels - 1 - labelNames[ "<id>" ];
private int getIndex( IdContext ctx, LabelIdx idx ) = getIndex( ctx, parse( #LabelIdx, "<idx>" ) ); // This is a cheat, because sometimes pattern matching fails
private int getIndex( idContext( _, _, funcNames, _, _, _, _, _, _, _ ), (FuncIdx)`<U32 id>` ) = toIntWasm( "<id>" );
private int getIndex( idContext( _, _, funcNames, _, _, _, _, _, _, _ ), (FuncIdx)`<Id id>` ) = funcNames[ "<id>" ];
private int getIndex( idContext( _, _, _, _, memNames, _, _, _, _, _ ), (MemIdx)`<U32 id>` ) = toIntWasm( "<id>" );
private int getIndex( idContext( _, _, _, _, memNames, _, _, _, _, _ ), (MemIdx)`<Id id>` ) = memNames[ "<id>" ];
private int getIndex( idContext( _, _, _, tableNames, _, _, _, _, _, _ ), (TableIdx)`<U32 id>` ) = toIntWasm( "<id>" );
private int getIndex( idContext( _, _, _, tableNames, _, _, _, _, _, _ ), (TableIdx)`<Id id>` ) = tableNames[ "<id>" ];

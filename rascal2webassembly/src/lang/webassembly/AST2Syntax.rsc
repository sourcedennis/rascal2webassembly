module lang::webassembly::AST2Syntax

import lang::webassembly::Syntax;
import lang::webassembly::Abstract;
import lang::webassembly::util::String2UTF8;
import util::Float;

import IO; // temp
import List;
import Map;
import ParseTree;

// Provides functions to convert a WebAssembly AST into its concrete
// representation.
//
// Public function(s):
//  - start[WebAssembly] toConcrete( MODULE m )
//
// The following naming conventions are used:
//  - $f[int] for function identifiers
//  - $t[int] for type identifiers
//  - $l[int] for labels
//  - $v[int] for local variables and parameters
//  - $m[int] for memories
//  - $g[int] for globals
//  - $b[int] for tables
//

// No need to keep variableIds or functionIds in here, as their index directly
// corresponds to their name. e.g. get_local( int id:5 ) => "get_local $v5"
private data ConcreteContext = ConcreteContext( map[int,TypeUse] types,
                                                int numLabels,
                                                // Keep the number of functions encountered thus far, such that
                                                // each function can be given the appropriate identifier
                                                int numFuncs,
                                                // Same for number of globals
                                                int numGlobals );
private alias Ctx = ConcreteContext;

public start[WebAssembly] toConcrete( MODULE m ) {
  list[ModuleField] fields = [];
  
  ConcreteContext ctx = ConcreteContext( ( ), 0, 0, 0 );
  
  for ( t <- m.types + m.imports + m.exports + m.funcs + m.mems + m.\data + m.tables + m.elems + m.globals ) {
    try {
      <ctx2,field> = toConcrete( ctx, t );
      ctx = ctx2;
      fields += field;
    } catch ParseError( loc l ): {
      println( "Inner parse error" );
      println( t );
      println( l );
      break;
    }
  }
  
  if ( \module( _, _, _, _, _, _, _, \start( FUNCIDX startIdx ), _, _ ) := m ) {
    FuncIdx conStartIdx = parse( #FuncIdx, "<startIdx>" );
    fields += (ModuleField)`(start <FuncIdx conStartIdx>)`;
  }
  
  start[WebAssembly] mCon = (start[WebAssembly])`(module)`;
  
  for ( f <- fields ) {
    if ( (start[WebAssembly])`(module <ModuleField *fs>)` := mCon ) {
      mCon = (start[WebAssembly])`(module <ModuleField *fs> <ModuleField f>)`;
    }
  }
  
  return mCon;
}

// ## Functypes
private tuple[Ctx,ModuleField] toConcrete( ctx:ConcreteContext( map[int,TypeUse] types, a, b, d ), FUNCTYPE t:functype( list[VALTYPE] paramTypes, list[VALTYPE] resultTypes ) ) {
  Id tIdx = parse( #Id, "$t<size( types )>" );
  
  list[Param] conParams = [ toParam( p, i ) | <p,i> <- zip( paramTypes, [0..size(paramTypes)] ) ];
  list[Result] conResults = [ toResult( r ) | r <- resultTypes ];
  
  FuncType fType = appendResults( appendParams( (FuncType)`(func)`, conParams ), conResults );
  Type tField = (Type)`(type <Id tIdx> <FuncType fType>)`;
  TypeUse tu = appendResults( appendParams( (TypeUse)`(type <Id tIdx>)`, conParams ), conResults );
  
  types[size(types)] = tu;
  
  return <ConcreteContext( types, a, b, d ), (ModuleField)`<Type tField>`>;
}

private FuncType appendParams( FuncType ft, list[Param] _:[ ] ) = ft;
private FuncType appendParams( (FuncType)`(func <Param* ps>)`, [ Param p, *L ] )
  = appendParams( (FuncType)`(func <Param* ps> <Param p>)`, L );

private TypeUse appendParams( TypeUse ft, list[Param] _:[ ] ) = ft;
private TypeUse appendParams( (TypeUse)`(type <TypeIdx tIdx>) <Param* ps>`, [ Param p, *L ] )
  = appendParams( (TypeUse)`(type <TypeIdx tIdx>) <Param* ps> <Param p>`, L );
  
private FuncType appendResults( FuncType ft, list[Result] _:[ ] ) = ft;
private FuncType appendResults( (FuncType)`(func <Param* ps> <Result* rs>)`, [ Result r, *L ] )
  = appendResults( (FuncType)`(func <Param* ps> <Result* rs> <Result r>)`, L );

private TypeUse appendResults( TypeUse ft, list[Result] _:[ ] ) = ft;
private TypeUse appendResults( (TypeUse)`(type <TypeIdx tIdx>) <Param* ps> <Result* rs>`, [ Result r, *L ] )
  = appendResults( (TypeUse)`(type <TypeIdx tIdx>) <Param* ps> <Result* rs> <Result r>`, L );
  
private Param toParam( VALTYPE t, int idx )
  = (Param)`(param <Id id> <ValType conType>)`
  when Id id := parse( #Id, "$v<idx>" ),
       ValType conType := toConcrete( t );

private Local toLocal( VALTYPE t, int idx )
  = (Local)`(local <Id id> <ValType conType>)`
  when Id id := parse( #Id, "$v<idx>" ),
       ValType conType := toConcrete( t );

private Result toResult( VALTYPE t )
  = (Result)`(result <ValType conType>)`
  when ValType conType := toConcrete( t );

private ValType toConcrete( VALTYPE _:i32( ) ) = (ValType)`i32`;
private ValType toConcrete( VALTYPE _:i64( ) ) = (ValType)`i64`;
private ValType toConcrete( VALTYPE _:f32( ) ) = (ValType)`f32`;
private ValType toConcrete( VALTYPE _:f64( ) ) = (ValType)`f64`;

// ## Imports

private tuple[Ctx,ModuleField] toConcrete( ConcreteContext ctx, IMPORT i:\import( NAME \module, NAME name, IMPORTDESC desc ) )
  = <ctx2, (ModuleField)`(import <Name cModule> <Name cName> <ImportDesc cDesc>)`>
  when cModule := parse( #String, fromPayload( \module ) ),
       cName := parse( #String, fromPayload( name ) ),
       <ctx2,cDesc> := toConcrete( ctx, desc );

private tuple[Ctx,ModuleField] toConcrete( ctx:ConcreteContext( a, b, numFuncs, c ), IMPORTDESC d:importdesc_func( TYPEIDX typeIdx ) )
  = < ConcreteContext( a, b, numFuncs + 1, c ), (ModuleField)`(func <Id? cId> <TypeUse cTypeUse>)`>
  when cId := parse( #Id, "$f<numFuncs>" ), cTypeUse := Ctx_getType( ctx, typeIdx );
  
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx, IMPORTDESC d:importdesc_table( TABLETYPE tt ) )
  // reply upon the fact that WebAssembly 1.0 only supports a single table
  = < ctx, (ModuleField)`(table $b0 <TableType conTt>)`>
  when conTt := toConcrete( tt );
  
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx, IMPORTDESC d:importdesc_mem( MEMTYPE mt ) )
  // reply upon the fact that WebAssembly 1.0 only supports a single memory
  = < ctx, (ModuleField)`(memory $m0 <MemType conMt>)`>
  when conMt := toConcrete( mt );

private tuple[Ctx,ModuleField] toConcrete( ctx:ConcreteContext( a, b, c, numGlobals ), IMPORTDESC d:importdesc_global( GLOBALTYPE gt ) )
  = < ConcreteContext( a, b, c, numGlobals + 1 ), (ModuleField)`(global <Id conId> <GlobalType conGt>)`>
  when conId := parse( #Id, "$g<numGlobals>" ), conGt := toConcrete( gt );

// ## Exports
private tuple[Ctx,ModuleField] toConcrete( ctx:ConcreteContext( a, b, numFuncs, d ), EXPORT e:\export( NAME name, EXPORTDESC desc ) ) {
  String conName = parse( #String, fromPayload( name ) );
  <ctx2,conDesc> = toConcrete( ctx, desc );
  return <ctx,(ModuleField)`(export <String conName> <ExportDesc conDesc>)`>;
}

private tuple[Ctx,ExportDesc] toConcrete( ctx:ConcreteContext( a, b, numFuncs, d ), EXPORTDESC e:exportdesc_func( FUNCIDX i ) )
  = <ConcreteContext( a, b, numFuncs + 1, d ), (ExportDesc)`(func <Id fIdx>)`>
  when Id fIdx := parse( #Id, "$f<i>" );

private tuple[Ctx,ExportDesc] toConcrete( ctx:ConcreteContext( a, b, c, d ), EXPORTDESC e:exportdesc_table( TABLEIDX i ) )
  = <ConcreteContext( a, b, c, d ), (ExportDesc)`(table <Id tIdx>)`>
  when Id tIdx := parse( #Id, "$b<i>" );
  
private tuple[Ctx,ExportDesc] toConcrete( ctx:ConcreteContext( a, b, c, d ), EXPORTDESC e:exportdesc_mem( MEMIDX i ) )
  = <ConcreteContext( a, b, c, d ), (ExportDesc)`(memory <Id mIdx>)`>
  when Id mIdx := parse( #Id, "$m<i>" );

private tuple[Ctx,ExportDesc] toConcrete( ctx:ConcreteContext( a, b, c, d ), EXPORTDESC e:exportdesc_global( GLOBALIDX i ) )
  = <ConcreteContext( a, b, c, d ), (ExportDesc)`(global <Id gIdx>)`>
  when Id gIdx := parse( #Id, "$g<i>" );
                
// ## Memory
// In WebAssembly 1.0 there can be at most 1 memory, so all of them are $m0.
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx, MEM _:mem( mt ) )
  = <ctx, (ModuleField)`(memory <Id conId> <MemType conMt>)`>
  when conId := parse( #Id, "$m0" ), conMt := toConcrete( mt );

private MemType toConcrete( memtype( LIMITS l ) )
  = (MemType)`<Limits conL>`
  when conL := toConcrete( l );

private Limits toConcrete( LIMITS _:limits( int min, int max ) )
  = (Limits)`<U32 conMin> <U32 conMax>`
  when conMin := parse( #U32, "<min>" ),
       conMax := parse( #U32, "<max>" );
       
private Limits toConcrete( LIMITS _:limits( int min ) )
  = (Limits)`<U32 conMin>`
  when conMin := parse( #U32, "<min>" );

// ## Data
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx, DATA _:\data( MEMIDX memIdx, EXPR offset, list[byte] init ) ) {
  MemIdx conMemIdx = parse( #MemIdx, "<memIdx>" );
  Expr conOffset = toConcrete( ctx, offset );
  String conInit = parse( #String, bytesToWasmString( init ) );
  return <ctx, (ModuleField)`(data <MemIdx conMemIdx> (offset <Expr conOffset>) <String conInit>)`>;
}

// ## Funcs

private tuple[Ctx,ModuleField] toConcrete( ctx:ConcreteContext( map[int,TypeUse] types, 0, numFuncs, d ), func( TYPEIDX typeIdx, list[VALTYPE] localTypes, expr( list[INSTR] adtInstrs ) ) ) {
  Id fIdx = parse( #Id, "$f<numFuncs>" );
  TypeUse tu = types[ typeIdx ];
  
  int numParams = countParams( types[ typeIdx ] );
  list[Local] locals = [ toLocal( p, i ) | <p,i> <- zip( localTypes, [numParams..numParams+size(localTypes)] ) ];
  Ctx subCtx = ConcreteContext(types,1,numFuncs, d);
  list[Instr] instrs = [ toConcrete( subCtx, adtInstr ) | adtInstr <- adtInstrs ];
  
  FuncBody funcBody = appendInstrs( appendLocals( (FuncBody)``, locals ), instrs );
  FuncFields funcFields = (FuncFields)`<TypeUse tu> <FuncBody funcBody>`;
  Func f = (Func)`(func <Id fIdx> <FuncFields funcFields>)`;
  
  return <ConcreteContext( types, 0, numFuncs+1, d ), (ModuleField)`<Func f>`>;
}

private int countParams( TypeUse t ) {
  int numParams = 0;
  visit ( t ) {
  case Param p: numParams += 1;
  }
  return numParams;
}

private FuncBody appendLocals( FuncBody fb, list[Local] _:[ ] ) = fb;
private FuncBody appendLocals( (FuncBody)`<Local* ls> <Instr* instrs>`, [ Local l, *L ] )
  = appendLocals( (FuncBody)`<Local* ls> <Local l> <Instr* instrs>`, L );

private FuncBody appendInstrs( FuncBody fb, list[Instr] instrs ) {
  for ( i <- instrs ) {
    if ( (FuncBody)`<Local* ls> <Instr* instrs>` := fb ) {
      fb = (FuncBody)`<Local* ls> <Instr* instrs> <Instr i>`;
    }
  }
  return fb;
}

// ## Tables
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx, table( tt ) ) {
  TableType conTt = toConcrete( tt );
  // WebAssembly only supports a single table, so it is called $b0
  return <ctx, (ModuleField)`(table $b0 <TableType conTt>)`>;
}

private TableType toConcrete( tabletype( LIMITS l, ELEMTYPE e ) ) {
  conLimits = toConcrete( l );
  conElemType = toConcrete( e );
  return (TableType)`<Limits conLimits> <ElemType conElemType>`;
}

private ElemType toConcrete( ELEMTYPE e:anyfunc( ) ) = (ElemType)`anyfunc`;

// ## Elements
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx, elem( TABLEIDX idx, EXPR offset, list[FUNCIDX] init ) ) {
  TableIdx tIdx = parse( #TableIdx, "<idx>" );
  Expr conExp = toConcrete( ctx, offset );
  list[FuncIdx] fIdxs = [ parse( #FuncIdx, "$f<i>" ) | i <- init ];
  return <ctx, appendFuncIdxs( (ModuleField)`(elem <TableIdx tIdx> (offset <Expr conExp>))`, fIdxs ) >;
}

private ModuleField appendFuncIdxs( ModuleField f, list[FuncIdx] _:[] ) = f;
private ModuleField appendFuncIdxs( (ModuleField)`(elem <TableIdx tIdx> (offset <Expr conExp>) <FuncIdx* fIdxs>)`, list[FuncIdx] _:[ fIdx, *L ] )
  = appendFuncIdxs( (ModuleField)`(elem <TableIdx tIdx> (offset <Expr conExp>) <FuncIdx* fIdxs> <FuncIdx fIdx>)`, L );

private Expr toConcrete( Ctx ctx, EXPR e:expr( list[INSTR] _:[] ) ) = (Expr)``;
private Expr toConcrete( Ctx ctx, EXPR e:expr( list[INSTR] _:[ i, *L ] ) )
  = (Expr)`<Instr conInstr> <Instr* conInstrs>`
  when conInstr := toConcrete( ctx, i ),
       (Expr)`<Instr* conInstrs>` := toConcrete( ctx, expr( L ) );

// ## Globals
private tuple[Ctx,ModuleField] toConcrete( Ctx ctx:ConcreteContext( a, b, c, numGlobals ), global( GLOBALTYPE gType, EXPR init ) ) {
  Id gId = parse( #Id, "$g<numGlobals>" );
  GlobalType conType = toConcrete( gType );
  Expr conInit = toConcrete( ctx, init );
  return <ConcreteContext(a,b,c,numGlobals+1), (ModuleField)`(global <Id gId> <GlobalType conType> <Expr conInit>)`>;
}

private GlobalType toConcrete( globaltype( MUT _:const( ), VALTYPE vt ) )
  = (GlobalType)`<ValType conValType>`
  when conValType := toConcrete( vt );

private GlobalType toConcrete( globaltype( MUT _:var( ), VALTYPE vt ) )
  = (GlobalType)`(mut <ValType conValType>)`
  when conValType := toConcrete( vt );

// Floats
private FN toConcrete( Float _:fval( real v ) ) = parse( #FN, "<v>" );
private FN toConcrete( Float _:canonical_nan( ) ) = parse( #FN, "nan" );
private FN toConcrete( Float _:arithmetic_nan( ) ) = parse( #FN, "nan:0x0" );
private FN toConcrete( Float _:positive_infinity( ) ) = parse( #FN, "+inf" );
private FN toConcrete( Float _:negative_infinity( ) ) = parse( #FN, "-inf" );
private FN toConcrete( Float _:arbitrary_infinity( ) ) = parse( #FN, "inf" );

// ## Instr
// consts
private Instr toConcrete( Ctx c, INSTR i:i32_const( v ) ) = (Instr)`i32.const <I32 conV>` when conV := parse( #I32, "<v>" );
private Instr toConcrete( Ctx c, INSTR i:i64_const( v ) ) = (Instr)`i64.const <I64 conV>` when conV := parse( #I64, "<v>" );
private Instr toConcrete( Ctx c, INSTR i:f32_const( v ) ) = (Instr)`f32.const <FN conV>` when conV := toConcrete( v );
private Instr toConcrete( Ctx c, INSTR i:f64_const( v ) ) = (Instr)`f64.const <FN conV>` when conV := toConcrete( v );
// iunop
private Instr toConcrete( Ctx c, INSTR i:i32_clz( ) ) = (Instr)`i32.clz`;
private Instr toConcrete( Ctx c, INSTR i:i32_ctz( ) ) = (Instr)`i32.ctz`;
private Instr toConcrete( Ctx c, INSTR i:i32_popcnt( ) ) = (Instr)`i32.popcnt`;
private Instr toConcrete( Ctx c, INSTR i:i64_clz( ) ) = (Instr)`i64.clz`;
private Instr toConcrete( Ctx c, INSTR i:i64_ctz( ) ) = (Instr)`i64.ctz`;
private Instr toConcrete( Ctx c, INSTR i:i64_popcnt( ) ) = (Instr)`i64.popcnt`;
// funop
private Instr toConcrete( Ctx c, INSTR i:f32_abs( ) ) = (Instr)`f32.abs`;
private Instr toConcrete( Ctx c, INSTR i:f32_neg( ) ) = (Instr)`f32.neg`;
private Instr toConcrete( Ctx c, INSTR i:f32_sqrt( ) ) = (Instr)`f32.sqrt`;
private Instr toConcrete( Ctx c, INSTR i:f32_ceil( ) ) = (Instr)`f32.ceil`;
private Instr toConcrete( Ctx c, INSTR i:f32_floor( ) ) = (Instr)`f32.floor`;
private Instr toConcrete( Ctx c, INSTR i:f32_trunc( ) ) = (Instr)`f32.trunc`;
private Instr toConcrete( Ctx c, INSTR i:f32_nearest( ) ) = (Instr)`f32.nearest`;
private Instr toConcrete( Ctx c, INSTR i:f64_abs( ) ) = (Instr)`f64.abs`;
private Instr toConcrete( Ctx c, INSTR i:f64_neg( ) ) = (Instr)`f64.neg`;
private Instr toConcrete( Ctx c, INSTR i:f64_sqrt( ) ) = (Instr)`f64.sqrt`;
private Instr toConcrete( Ctx c, INSTR i:f64_ceil( ) ) = (Instr)`f64.ceil`;
private Instr toConcrete( Ctx c, INSTR i:f64_floor( ) ) = (Instr)`f64.floor`;
private Instr toConcrete( Ctx c, INSTR i:f64_trunc( ) ) = (Instr)`f64.trunc`;
private Instr toConcrete( Ctx c, INSTR i:f64_nearest( ) ) = (Instr)`f64.nearest`;
// ibinop
private Instr toConcrete( Ctx c, INSTR i:i32_add( ) ) = (Instr)`i32.add`;
private Instr toConcrete( Ctx c, INSTR i:i32_sub( ) ) = (Instr)`i32.sub`;
private Instr toConcrete( Ctx c, INSTR i:i32_mul( ) ) = (Instr)`i32.mul`;
private Instr toConcrete( Ctx c, INSTR i:i32_div_u( ) ) = (Instr)`i32.div_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_div_s( ) ) = (Instr)`i32.div_s`;
private Instr toConcrete( Ctx c, INSTR i:i32_rem_u( ) ) = (Instr)`i32.rem_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_rem_s( ) ) = (Instr)`i32.rem_s`;
private Instr toConcrete( Ctx c, INSTR i:i32_and( ) ) = (Instr)`i32.and`;
private Instr toConcrete( Ctx c, INSTR i:i32_or( ) ) = (Instr)`i32.or`;
private Instr toConcrete( Ctx c, INSTR i:i32_xor( ) ) = (Instr)`i32.xor`;
private Instr toConcrete( Ctx c, INSTR i:i32_shl( ) ) = (Instr)`i32.shl`;
private Instr toConcrete( Ctx c, INSTR i:i32_shr_u( ) ) = (Instr)`i32.shr_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_shr_s( ) ) = (Instr)`i32.shr_s`;
private Instr toConcrete( Ctx c, INSTR i:i32_rotl( ) ) = (Instr)`i32.rotl`;
private Instr toConcrete( Ctx c, INSTR i:i32_rotr( ) ) = (Instr)`i32.rotr`;
private Instr toConcrete( Ctx c, INSTR i:i64_add( ) ) = (Instr)`i64.add`;
private Instr toConcrete( Ctx c, INSTR i:i64_sub( ) ) = (Instr)`i64.sub`;
private Instr toConcrete( Ctx c, INSTR i:i64_mul( ) ) = (Instr)`i64.mul`;
private Instr toConcrete( Ctx c, INSTR i:i64_div_u( ) ) = (Instr)`i64.div_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_div_s( ) ) = (Instr)`i64.div_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_rem_u( ) ) = (Instr)`i64.rem_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_rem_s( ) ) = (Instr)`i64.rem_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_and( ) ) = (Instr)`i64.and`;
private Instr toConcrete( Ctx c, INSTR i:i64_or( ) ) = (Instr)`i64.or`;
private Instr toConcrete( Ctx c, INSTR i:i64_xor( ) ) = (Instr)`i64.xor`;
private Instr toConcrete( Ctx c, INSTR i:i64_shl( ) ) = (Instr)`i64.shl`;
private Instr toConcrete( Ctx c, INSTR i:i64_shr_u( ) ) = (Instr)`i64.shr_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_shr_s( ) ) = (Instr)`i64.shr_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_rotl( ) ) = (Instr)`i64.rotl`;
private Instr toConcrete( Ctx c, INSTR i:i64_rotr( ) ) = (Instr)`i64.rotr`;
// fbinop
private Instr toConcrete( Ctx c, INSTR i:f32_add( ) ) = (Instr)`f32.add`;
private Instr toConcrete( Ctx c, INSTR i:f32_sub( ) ) = (Instr)`f32.sub`;
private Instr toConcrete( Ctx c, INSTR i:f32_mul( ) ) = (Instr)`f32.mul`;
private Instr toConcrete( Ctx c, INSTR i:f32_div( ) ) = (Instr)`f32.div`;
private Instr toConcrete( Ctx c, INSTR i:f32_min( ) ) = (Instr)`f32.min`;
private Instr toConcrete( Ctx c, INSTR i:f32_max( ) ) = (Instr)`f32.max`;
private Instr toConcrete( Ctx c, INSTR i:f32_copysign( ) ) = (Instr)`f32.copysign`;
private Instr toConcrete( Ctx c, INSTR i:f64_add( ) ) = (Instr)`f64.add`;
private Instr toConcrete( Ctx c, INSTR i:f64_sub( ) ) = (Instr)`f64.sub`;
private Instr toConcrete( Ctx c, INSTR i:f64_mul( ) ) = (Instr)`f64.mul`;
private Instr toConcrete( Ctx c, INSTR i:f64_div( ) ) = (Instr)`f64.div`;
private Instr toConcrete( Ctx c, INSTR i:f64_min( ) ) = (Instr)`f64.min`;
private Instr toConcrete( Ctx c, INSTR i:f64_max( ) ) = (Instr)`f64.max`;
private Instr toConcrete( Ctx c, INSTR i:f64_copysign( ) ) = (Instr)`f64.copysign`;
// itestop
private Instr toConcrete( Ctx c, INSTR i:i32_eqz( ) ) = (Instr)`i32.eqz`;
private Instr toConcrete( Ctx c, INSTR i:i64_eqz( ) ) = (Instr)`i64.eqz`;
// irelop
private Instr toConcrete( Ctx c, INSTR i:i32_eq( ) ) = (Instr)`i32.eq`;
private Instr toConcrete( Ctx c, INSTR i:i32_ne( ) ) = (Instr)`i32.ne`;
private Instr toConcrete( Ctx c, INSTR i:i32_lt_u( ) ) = (Instr)`i32.lt_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_lt_s( ) ) = (Instr)`i32.lt_s`;
private Instr toConcrete( Ctx c, INSTR i:i32_gt_u( ) ) = (Instr)`i32.gt_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_gt_s( ) ) = (Instr)`i32.gt_s`;
private Instr toConcrete( Ctx c, INSTR i:i32_le_u( ) ) = (Instr)`i32.le_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_le_s( ) ) = (Instr)`i32.le_s`;
private Instr toConcrete( Ctx c, INSTR i:i32_ge_u( ) ) = (Instr)`i32.ge_u`;
private Instr toConcrete( Ctx c, INSTR i:i32_ge_s( ) ) = (Instr)`i32.ge_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_eq( ) ) = (Instr)`i64.eq`;
private Instr toConcrete( Ctx c, INSTR i:i64_ne( ) ) = (Instr)`i64.ne`;
private Instr toConcrete( Ctx c, INSTR i:i64_lt_u( ) ) = (Instr)`i64.lt_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_lt_s( ) ) = (Instr)`i64.lt_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_gt_u( ) ) = (Instr)`i64.gt_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_gt_s( ) ) = (Instr)`i64.gt_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_le_u( ) ) = (Instr)`i64.le_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_le_s( ) ) = (Instr)`i64.le_s`;
private Instr toConcrete( Ctx c, INSTR i:i64_ge_u( ) ) = (Instr)`i64.ge_u`;
private Instr toConcrete( Ctx c, INSTR i:i64_ge_s( ) ) = (Instr)`i64.ge_s`;
// frelop
private Instr toConcrete( Ctx c, INSTR i:f32_eq( ) ) = (Instr)`f32.eq`;
private Instr toConcrete( Ctx c, INSTR i:f32_ne( ) ) = (Instr)`f32.ne`;
private Instr toConcrete( Ctx c, INSTR i:f32_lt( ) ) = (Instr)`f32.lt`;
private Instr toConcrete( Ctx c, INSTR i:f32_gt( ) ) = (Instr)`f32.gt`;
private Instr toConcrete( Ctx c, INSTR i:f32_le( ) ) = (Instr)`f32.le`;
private Instr toConcrete( Ctx c, INSTR i:f32_ge( ) ) = (Instr)`f32.ge`;
private Instr toConcrete( Ctx c, INSTR i:f64_eq( ) ) = (Instr)`f64.eq`;
private Instr toConcrete( Ctx c, INSTR i:f64_ne( ) ) = (Instr)`f64.ne`;
private Instr toConcrete( Ctx c, INSTR i:f64_lt( ) ) = (Instr)`f64.lt`;
private Instr toConcrete( Ctx c, INSTR i:f64_gt( ) ) = (Instr)`f64.gt`;
private Instr toConcrete( Ctx c, INSTR i:f64_le( ) ) = (Instr)`f64.le`;
private Instr toConcrete( Ctx c, INSTR i:f64_ge( ) ) = (Instr)`f64.ge`;
//
private Instr toConcrete( Ctx c, INSTR i:i32_wrap_i64( ) ) = (Instr)`i32.wrap/i64`;
private Instr toConcrete( Ctx c, INSTR i:i64_extend_u_i32( ) ) = (Instr)`i64.extend_u/i32`;
private Instr toConcrete( Ctx c, INSTR i:i64_extend_s_i32( ) ) = (Instr)`i64.extend_s/i32`;
private Instr toConcrete( Ctx c, INSTR i:i32_trunc_u_f32( ) ) = (Instr)`i32.trunc_u/f32`;
private Instr toConcrete( Ctx c, INSTR i:i32_trunc_s_f32( ) ) = (Instr)`i32.trunc_s/f32`;
private Instr toConcrete( Ctx c, INSTR i:i32_trunc_u_f64( ) ) = (Instr)`i32.trunc_u/f64`;
private Instr toConcrete( Ctx c, INSTR i:i32_trunc_s_f64( ) ) = (Instr)`i32.trunc_s/f64`;
private Instr toConcrete( Ctx c, INSTR i:i64_trunc_u_f32( ) ) = (Instr)`i64.trunc_u/f32`;
private Instr toConcrete( Ctx c, INSTR i:i64_trunc_s_f32( ) ) = (Instr)`i64.trunc_s/f32`;
private Instr toConcrete( Ctx c, INSTR i:i64_trunc_u_f64( ) ) = (Instr)`i64.trunc_u/f64`;
private Instr toConcrete( Ctx c, INSTR i:i64_trunc_s_f64( ) ) = (Instr)`i64.trunc_s/f64`;
private Instr toConcrete( Ctx c, INSTR i:f32_demote_f64( ) ) = (Instr)`f32.demote/f64`;
private Instr toConcrete( Ctx c, INSTR i:f64_promote_f32( ) ) = (Instr)`f64.promote/f32`;
private Instr toConcrete( Ctx c, INSTR i:f32_convert_u_i32( ) ) = (Instr)`f32.convert_u/i32`;
private Instr toConcrete( Ctx c, INSTR i:f32_convert_s_i32( ) ) = (Instr)`f32.convert_s/i32`;
private Instr toConcrete( Ctx c, INSTR i:f32_convert_u_i64( ) ) = (Instr)`f32.convert_u/i64`;
private Instr toConcrete( Ctx c, INSTR i:f32_convert_s_i64( ) ) = (Instr)`f32.convert_s/i64`;
private Instr toConcrete( Ctx c, INSTR i:f64_convert_u_i32( ) ) = (Instr)`f64.convert_u/i32`;
private Instr toConcrete( Ctx c, INSTR i:f64_convert_s_i32( ) ) = (Instr)`f64.convert_s/i32`;
private Instr toConcrete( Ctx c, INSTR i:f64_convert_u_i64( ) ) = (Instr)`f64.convert_u/i64`;
private Instr toConcrete( Ctx c, INSTR i:f64_convert_s_i64( ) ) = (Instr)`f64.convert_s/i64`;
private Instr toConcrete( Ctx c, INSTR i:i32_reinterpret_f32( ) ) = (Instr)`i32.reinterpret/f32`;
private Instr toConcrete( Ctx c, INSTR i:i64_reinterpret_f64( ) ) = (Instr)`i64.reinterpret/f64`;
private Instr toConcrete( Ctx c, INSTR i:f32_reinterpret_i32( ) ) = (Instr)`f32.reinterpret/i32`;
private Instr toConcrete( Ctx c, INSTR i:f64_reinterpret_i64( ) ) = (Instr)`f64.reinterpret/i64`;
// parametric instructions
private Instr toConcrete( Ctx c, INSTR i:drop( ) ) = (Instr)`drop`;
private Instr toConcrete( Ctx c, INSTR i:select( ) ) = (Instr)`select`;
// variable instructions
private Instr toConcrete( Ctx c, INSTR i:get_local( idx ) ) = (Instr)`get_local <Id conId>` when conId := parse( #Id, "$v<idx>" );
private Instr toConcrete( Ctx c, INSTR i:set_local( idx ) ) = (Instr)`set_local <Id conId>` when conId := parse( #Id, "$v<idx>" );
private Instr toConcrete( Ctx c, INSTR i:tee_local( idx ) ) = (Instr)`tee_local <Id conId>` when conId := parse( #Id, "$v<idx>" );
private Instr toConcrete( Ctx c, INSTR i:get_global( idx ) ) = (Instr)`get_global <Id conId>` when conId := parse( #Id, "$g<idx>" );
private Instr toConcrete( Ctx c, INSTR i:set_global( idx ) ) = (Instr)`set_global <Id conId>` when conId := parse( #Id, "$g<idx>" );
// memory instructions
private int NATALIGN_8  = 0; // 2^0 = 1 bytes = 8 bits
private int NATALIGN_16 = 1; // 2^1 = 2 bytes = 16 bits
private int NATALIGN_32 = 2; // 2^2 = 4 bytes = 32 bits
private int NATALIGN_64 = 3; // 2^3 = 8 bytes = 64 bits
private Instr toConcrete( Ctx c, INSTR i:i32_load( offset, align ) ) = (Instr)`i32.load <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:i64_load( offset, align ) ) = (Instr)`i64.load <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_64 );
private Instr toConcrete( Ctx c, INSTR i:f32_load( offset, align ) ) = (Instr)`f32.load <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:f64_load( offset, align ) ) = (Instr)`f64.load <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_64 );
private Instr toConcrete( Ctx c, INSTR i:i32_store( offset, align ) ) = (Instr)`i32.store <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:i64_store( offset, align ) ) = (Instr)`i64.store <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_64 );
private Instr toConcrete( Ctx c, INSTR i:f32_store( offset, align ) ) = (Instr)`f32.store <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:f64_store( offset, align ) ) = (Instr)`f64.store <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_64 );
private Instr toConcrete( Ctx c, INSTR i:i32_load8_u( offset, align ) ) = (Instr)`i32.load8_u <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_8 );
private Instr toConcrete( Ctx c, INSTR i:i32_load8_s( offset, align ) ) = (Instr)`i32.load8_s <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_8 );
private Instr toConcrete( Ctx c, INSTR i:i64_load8_u( offset, align ) ) = (Instr)`i64.load8_u <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_8 );
private Instr toConcrete( Ctx c, INSTR i:i64_load8_s( offset, align ) ) = (Instr)`i64.load8_s <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_8 );
private Instr toConcrete( Ctx c, INSTR i:i32_load16_u( offset, align ) ) = (Instr)`i32.load16_u <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_16 );
private Instr toConcrete( Ctx c, INSTR i:i32_load16_s( offset, align ) ) = (Instr)`i32.load16_s <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_16 );
private Instr toConcrete( Ctx c, INSTR i:i64_load16_u( offset, align ) ) = (Instr)`i64.load16_u <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_16 );
private Instr toConcrete( Ctx c, INSTR i:i64_load16_s( offset, align ) ) = (Instr)`i64.load16_s <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_16 );
private Instr toConcrete( Ctx c, INSTR i:i64_load32_u( offset, align ) ) = (Instr)`i64.load32_u <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:i64_load32_s( offset, align ) ) = (Instr)`i64.load32_s <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:i32_store8( offset, align ) ) = (Instr)`i32.store8 <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_8 );
private Instr toConcrete( Ctx c, INSTR i:i64_store8( offset, align ) ) = (Instr)`i64.store8 <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_8 );
private Instr toConcrete( Ctx c, INSTR i:i32_store16( offset, align ) ) = (Instr)`i32.store16 <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_16 );
private Instr toConcrete( Ctx c, INSTR i:i64_store16( offset, align ) ) = (Instr)`i64.store16 <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_16 );
private Instr toConcrete( Ctx c, INSTR i:i64_store32( offset, align ) ) = (Instr)`i64.store32 <MemArg memArg>` when memArg := toConcreteMemArg( offset, align, NATALIGN_32 );
private Instr toConcrete( Ctx c, INSTR i:memory_size( ) ) = (Instr)`memory.size`;
private Instr toConcrete( Ctx c, INSTR i:memory_grow( ) ) = (Instr)`memory.grow`;
// control instructions
private Instr toConcrete( Ctx c, INSTR i:nop( ) ) = (Instr)`nop`;
private Instr toConcrete( Ctx c, INSTR i:unreachable( ) ) = (Instr)`unreachable`;

private Instr toConcrete( Ctx c:ConcreteContext( types, numLabels, numFuncs, numGlobals ), INSTR i:block( resType, instrs ) )
  = appendInstrs( (Instr)`block <Id l> <ResultType conResType> end <Id l>`, conInstrs )
  when l := parse( #Id, "$l<numLabels>" ),
       conResType := toConcrete( resType ),
       ctx2 := ConcreteContext( types, numLabels + 1, numFuncs, numGlobals ),
       conInstrs := [ toConcrete( ctx2, instr ) | instr <- instrs ];
       
private Instr toConcrete( Ctx c:ConcreteContext( types, numLabels, numFuncs, numGlobals ), INSTR i:loop( resType, instrs ) )
  = appendInstrs( (Instr)`loop <Id l> <ResultType conResType> end <Id l>`, conInstrs )
  when l := parse( #Id, "$l<numLabels>" ),
       conResType := toConcrete( resType ),
       ctx2 := ConcreteContext( types, numLabels + 1, numFuncs, numGlobals ),
       conInstrs := [ toConcrete( ctx2, instr ) | instr <- instrs ];
       
private Instr toConcrete( Ctx c:ConcreteContext( types, numLabels, numFuncs, numGlobals ), INSTR i:\if( RESULTTYPE resType, list[INSTR] ifInstrs, list[INSTR] elseInstrs ) )
  // "if" Label ResultType Instr* "else" Id? Instr* "end" Id?
  = appendElseInstrs( appendIfInstrs( (Instr)`if <Id l> <ResultType conResType> else end <Id l>`, conIfInstrs ), conElseInstrs )
  when l := parse( #Id, "$l<numLabels>" ),
       conResType := toConcrete( resType ),
       ctx2 := ConcreteContext( types, numLabels + 1, numFuncs, numGlobals ),
       conIfInstrs := [ toConcrete( ctx2, instr ) | instr <- ifInstrs ],
       conElseInstrs := [ toConcrete( ctx2, instr ) | instr <- elseInstrs ];

private Instr toConcrete( Ctx c, INSTR i:br( int idx ) ) = (Instr)`br <LabelIdx id>`
  when LabelIdx id := toLabelIdx( c, idx );
private Instr toConcrete( Ctx c, INSTR i:br_if( int idx ) ) = (Instr)`br_if <LabelIdx id>`
  when LabelIdx id := toLabelIdx( c, idx );
  
private Instr toConcrete( Ctx c, INSTR i:br_table( list[int] tableIdxs, int idx ) ) {
  id = toLabelIdx( c, idx );
  tableIds = [ toLabelIdx( c, tIdx ) | tIdx <- tableIdxs ];
  return appendTableIdxs( (Instr)`br_table <Id id>`, tableIds );
}

private LabelIdx toLabelIdx( Ctx c:ConcreteContext( a, numLabels, b, d ), int idx ) {
  if ( numLabels - idx - 1 == 0 ) {
    // This label references the function block itself, which has no label id
    return parse( #LabelIdx, "<idx>" );
  } else {
    return parse( #LabelIdx, "$l<numLabels - idx - 1>" );
  }
}

private Instr toConcrete( Ctx c, INSTR i:\return( ) ) = (Instr)`return`;
private Instr toConcrete( Ctx c, INSTR i:\call( int fIdx ) ) = (Instr)`call <Id conFIdx>` when conFIdx := parse( #Id, "$f<fIdx>" );

private Instr toConcrete( Ctx c:ConcreteContext( types, b, c, d ), INSTR i:\call_indirect( TYPEIDX idx ) )
  = (Instr)`call_indirect <TypeUse tu>`
  when tu := types[ idx ];

private Instr appendTableIdxs( i:(Instr)`br_table <LabelIdx* labelIdxs> <LabelIdx defaultIdx>`, list[LabelIdx] _:[ ] ) = i;
private Instr appendTableIdxs( (Instr)`br_table <LabelIdx* labelIdxs> <LabelIdx defaultIdx>`, list[LabelIdx] _:[ id, *L ] )
  = appendTableIdxs( (Instr)`br_table <LabelIdx* labelIdxs> <LabelIdx id> <LabelIdx defaultIdx>`, L );

// These things are iterative, because a recursive alternative causes Stack overflows regularly

private Instr appendInstrs( Instr i:(Instr)`block <Id l> <ResultType conResType> <Instr* conInstrs> end <Id l>`, list[Instr] instrs ) {
  for ( instr <- instrs ) {
    if ( (Instr)`block <Id l> <ResultType conResType> <Instr* conInstrs> end <Id l>` := i ) {
      i = (Instr)`block <Id l> <ResultType conResType> <Instr* conInstrs> <Instr instr> end <Id l>`;
    }
  }
  return i;
}

private Instr appendInstrs( Instr i:(Instr)`loop <Id l> <ResultType conResType> <Instr* conInstrs> end <Id l>`, list[Instr] instrs ) {
  for ( instr <- instrs ) {
    if ( (Instr)`loop <Id l> <ResultType conResType> <Instr* conInstrs> end <Id l>` := i ) {
      i = (Instr)`loop <Id l> <ResultType conResType> <Instr* conInstrs> <Instr instr> end <Id l>`;
    }
  }
  return i;
}

private Instr appendIfInstrs( Instr i:(Instr)`if <Id l> <ResultType conResType> <Instr *ifInstrs> else end <Id l>`, list[Instr] instrs ) {
  for ( instr <- instrs ) {
    if ( (Instr)`if <Id l> <ResultType conResType> <Instr *ifInstrs> else end <Id l>` := i ) {
      i = (Instr)`if <Id l> <ResultType conResType> <Instr *ifInstrs> <Instr instr> else end <Id l>`;
    }
  }
  return i;
}

private Instr appendElseInstrs( Instr i:(Instr)`if <Id l> <ResultType conResType> <Instr *ifInstrs> else <Instr* elseInstrs> end <Id l>`, list[Instr] instrs ) {
  for ( instr <- instrs ) {
    if ( (Instr)`if <Id l> <ResultType conResType> <Instr *ifInstrs> else <Instr* elseInstrs> end <Id l>` := i ) {
      i = (Instr)`if <Id l> <ResultType conResType> <Instr *ifInstrs> else <Instr* elseInstrs> <Instr instr> end <Id l>`;
    }
  }
  return i;
}

private ResultType toConcrete( RESULTTYPE t:resulttype( [] ) ) = (ResultType)``;
private ResultType toConcrete( RESULTTYPE t:resulttype( [VALTYPE vt] ) )
  = (ResultType)`<Result conVt>` when conVt := toResult( vt );
private ResultType toConcrete( RESULTTYPE t:resulttype( list[VALTYPE] vts ) ) {
  throw IllegalArgument( vts, "The syntax does not allow multiple result types yet" );
}


private MemArg toConcreteMemArg( int offset, int align, int naturalAlign )
  = (MemArg)`<Offset conOffset> <Align conAlign>`
  when Offset conOffset := toConcreteOffset( offset ),
       Align conAlign := toConcreteAlign( align, naturalAlign );

private Offset toConcreteOffset( int offset:0 ) = (Offset)``;
private Offset toConcreteOffset( int offset ) = parse( #Offset, "offset=<offset>" );

private Align toConcreteAlign( int align, int naturalAlign )
  = ( align == naturalAlign ) ? (Align)`` : parse( #Align, "align=<align>" );

// This should not happen
private Instr toConcrete( Ctx c, INSTR i ) {
  println( "Missing instruction implementation: <i>" );
  return (Instr)`nop`;
}

private TypeUse Ctx_getType( Ctx c:ConcreteContext( types, _, _, _ ), TYPEIDX typeIdx )
  = types[ typeIdx ];
 
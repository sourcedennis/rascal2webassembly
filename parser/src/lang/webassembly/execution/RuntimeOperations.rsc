module lang::webassembly::execution::RuntimeOperations

import util::Maybe;
import util::Math;
import Exception;
import List;
import IO; // temp

import lang::webassembly::execution::RuntimeStructures;
import lang::webassembly::ADT;
import util::LittleEndian;
import util::Float;

// This module defines operations on the defined runtime structures
//
// Public operations:
// - moduleinst setupModuleInstance( MODULE m )
// - config setupExecutionConfig( MODULE modBase, moduleinst modInst, int funcIdx, list[runtime_val] params )
// - FUNCIDX findStartFuncIdx( MODULE m )
// - FUNCIDX findExportFuncIdx( MODULE m, str name )
// - config reduce( config c, list[runtime_val]( str moduleName, str funcName, list[runtime_val] params ) hostFunction )
// - bool isDone( config c )
// - bool hasTrapped( config c )
// - list[runtime_val] getResults( config c )

// Note: Importing of tables/globals is not supported

// # Settings

// The maximum number of memory pages (sized 64KiB) that can be present in a store
// Not formally from the specification, as semantically, memory would be infinite.
// Clearly, no infinite memory is available. And as stores are immutable, and copied
// and modified upon performing some reduction steps, memory quickly runs out
// resulting in a OutOfMemoryError. Hence this variable exists.
// Also, an 0x100*(64*1024) array of Rascal's big integers, is a lot bigger than
// a byte array of the same size. So this is a reasonable amount.
int MAX_NUM_PAGES = 0x100;

// ## Configuration

public bool isDone( config( s, thread( Stack stack, [] ) ) ) = true;
public bool isDone( config( s, thread( Stack stack, [ sec( trap( ) ) ] ) ) ) = true;
public bool isDone( config( s, thread( Stack stack, list[instrelem] instructions ) ) ) = false;

list[runtime_val] getResults( config c: config( store, thread( Stack stack, instructions ) ) )
  = [ v | sev( v ) <- stack ]
  when isDone( c ) && !hasTrapped( c );
list[runtime_val] getResults( config c ) = \throw( AssertionFailed( "Configuration not succesfully finished" ) );

public bool hasTrapped( config( s, thread( Stack stack, [ sec( trap( ) ) ] ) ) ) = true;
public bool hasTrapped( config c ) = \throw( AssertionFailed( "Configuration not finished" ) )
  when !isDone( c );
public bool hasTrapped( config c ) = false;

public moduleinst setupModuleInstance( m:\module( types, list[FUNC] funcs, tables, mems, globals, elems, \data, START st, imports, exports ) )
  = setupModuleInstance( \module( types, funcs, tables, mems, globals, elems, \data, imports, exports ) ); // No start

public moduleinst setupModuleInstance( m:\module( types, list[FUNC] funcs, tables, mems, globals, elems, \data, imports, exports ) )
  // For this implementation, any "addr" directly corresponds to its index in the module's list
  = moduleinst( types, [0..size(funcs)+size(getImportFuncs(imports))], [0..size(tables)+size(getImportTables(imports))], [0..size(mems)], [0..size(globals)], [ setupExportInst( e ) | e <- exports ] );

public store setupStore( MODULE modBase, moduleinst modInst )
  = store( funcInsts, tableInsts, memInsts, globalInsts )
  when funcInsts := setupImportFuncInsts( modInst, modBase.types, modBase.imports ) + setupFuncInsts( modInst, modBase.types, modBase.funcs ),
       tableInsts := setupTableinsts( getImportTables( modBase.imports ), modBase.tables, modBase.elems ),
       memInsts := setupMeminsts( getImportMems( modBase.imports ), modBase.mems, modBase.\data ),
       globalInsts := toGlobalinsts( modBase.globals );

public config setupExecutionConfig( MODULE modBase, moduleinst modInst, store s, int funcIdx, list[runtime_val] arguments ) {
  importFuncs = getImportFuncs(modBase.imports);
  if ( funcIdx < size(importFuncs) ) {
    throw IllegalArgument( "Cannot have an imported function as start function" );
  }
  
  FUNC func = modBase.funcs[ funcIdx - size( importFuncs ) ];
  list[VALTYPE] params = modBase.types[ func.typeIdx ].params;
  if ( params != [ getType( a ) | a <- arguments ] ) {
    throw IllegalArgument( "Function arguments do not match function signature: <params>" );
  }
  
  locals = [ runtimeInit( lt ) | lt <- func.locals ];
  frame f = frame( arguments + locals, modInst );
  int retArity = size( modBase.types[ func.typeIdx ].results );
  instrBlock = block( resulttype( modBase.types[ func.typeIdx ].results ), func.body.instrs );
  return config( s, thread( [ sef( retArity, f ) ], [ sei( instrBlock ), sec( end( ) ) ] ) );
}

public bool isImportedFunc( MODULE modBase, FUNCIDX idx )
  = idx < size(importFuncs)
  when importFuncs := getImportFuncs(modBase.imports);

private VALTYPE getType( runtime_val v: i32( _ ) ) = i32( );
private VALTYPE getType( runtime_val v: i64( _ ) ) = i64( );
private VALTYPE getType( runtime_val v: f32( _ ) ) = f32( );
private VALTYPE getType( runtime_val v: f64( _ ) ) = f64( );

FUNCIDX findStartFuncIdx( MODULE modBase:\module( _, _, _, _, _, _, _, START \start, _, _ ) ) = findStartFuncIdx( \start );
FUNCIDX findStartFuncIdx( \start( FUNCIDX funcIdx ) ) = funcIdx;
FUNCIDX findStartFuncIdx( MODULE modBase ) = -1;

// ## Exports
FUNCIDX findExportFuncIdx( MODULE m, str name ) = findExportFuncIdx( m.exports, name );
FUNCIDX findExportFuncIdx( exports:[ export( eName, exportdesc_func( FUNCIDX i ) ), *L ], str name )
  = ( ( eName == name ) ? i : findExportFuncIdx( L, name ) );
FUNCIDX findExportFuncIdx( exports:[ _, *L ], str name ) = findExportFuncIdx( L, name );
FUNCIDX findExportFuncIdx( exports:[], str name ) = -1;

runtime_val findExportGlobalVal( moduleinst modInst, store s, str name )
  = s.globals[ addr ].val
  when addr := findExportGlobalAddr( modInst, name );

/*GLOBALIDX findExportGlobalIdx( MODULE m, str name ) = findExportGlobalIdx( m.exports, name );
GLOBALIDX findExportGlobalIdx( exports:[ export( eName, exportdesc_global( GLOBALIDX i ) ), *L ], str name )
  = ( ( eName == name ) ? i : findExportGlobalIdx( L, name ) );
GLOBALIDX findExportGlobalIdx( exports:[ _, *L ], str name ) = findExportFuncIdx( L, name );
GLOBALIDX findExportGlobalIdx( exports:[], str name ) = -1;*/

private globaladdr findExportGlobalAddr( list[exportinst] _:[], str name ) = \throw( AssertionFailed( "Global not present" ) );
// Doing the pattern-match on exportinst in the function signature curiously fails. TODO?
private globaladdr findExportGlobalAddr( list[exportinst] insts, str name ) {
  if ( exportinst( str eName, externval_global( addr ) ) := head( insts ) && eName == name ) {
    return addr;
  } else {
    return findExportGlobalAddr( tail( insts ), name );
  }
}

public globaladdr findExportGlobalAddr( moduleinst modInst, str name )
  = findExportGlobalAddr( modInst.exports, name );

// In this implementation the "addr" directly corresponds to the index in the module. This is therefore assumed here
exportinst setupExportInst( export( NAME name, exportdesc_func( FUNCIDX i ) ) ) = exportinst( name, externval_func( i ) );
exportinst setupExportInst( export( NAME name, exportdesc_table( FUNCIDX i ) ) ) = exportinst( name, externval_table( i ) );
exportinst setupExportInst( export( NAME name, exportdesc_mem( FUNCIDX i ) ) ) = exportinst( name, externval_mem( i ) );
exportinst setupExportInst( export( NAME name, exportdesc_global( FUNCIDX i ) ) ) = exportinst( name, externval_global( i ) );

// ## Imports
private list[IMPORT] getImportFuncs( list[IMPORT] imports )
  = [ i | i <- imports, \import( _, _, importdesc_func( _ ) ) := i ];
  
private list[IMPORT] getImportTables( list[IMPORT] imports )
  = [ i | i <- imports, \import( _, _, importdesc_table( _ ) ) := i ];
  
private list[IMPORT] getImportMems( list[IMPORT] imports )
  = [ i | i <- imports, \import( _, _, importdesc_mem( _ ) ) := i ];
  
private list[funcinst] setupImportFuncInsts( moduleinst m, list[FUNCTYPE] functypes, list[IMPORT] imports )
  = [ toFuncinst( m, functypes, i ) | i <- getImportFuncs( imports ) ];

private list[funcinst] setupFuncInsts( moduleinst m, list[FUNCTYPE] functypes, list[FUNC] funcs )
  = [ toFuncinst( m, functypes, f ) | f <- funcs ];

// ## Functions
private funcinst toFuncinst( moduleinst m, list[FUNCTYPE] functypes, \import( NAME \moduleName, NAME funcName, importdesc_func( typeIdx ) ) )
  = funcinst( functypes[ typeIdx ], \moduleName, funcName );

private funcinst toFuncinst( moduleinst m, list[FUNCTYPE] functypes, f:func( typeIdx, _, _ ) )
  = funcinst( functypes[ typeIdx ], m, f );

//list[INSTR] getFuncInstrs( func( _, _, expr( instrs ) ) ) = instrs;

// ## Memory
// Note: WebAssembly 1.0 only supports at most 1 memory page

private int PAGESIZE = 64 * 1024; // from spec

public store setMemoryBytes( s:store( a, b, list[meminst] mems, c ), int location, bytes values )
  = store( a, b, setAt( mems, 0, setMemoryBytes( mems[ 0 ], location, values ) ), c )
  when hasMemoryBytes( s, location, size( values ) );

private meminst setMemoryBytes( m:meminst( bytes \data, int maxNumPages ), int location, bytes values ) {
  return meminst( mergeAt( \data, location, values ), maxNumPages );
}
private meminst setMemoryBytes( meminst( bytes \data ), int location, bytes values )
  = meminst( mergeAt( \data, location, values ) );

public bytes getMemoryBytes( s:store( a, b, list[meminst] mems, c ), int location, int numBytes )
  = mems[ 0 ].\data[location..location+numBytes]
  when hasMemoryBytes( s, location, numBytes );

public bool hasMemoryBytes( store( a, b, list[meminst] mems, c ), int location, int numBytes )
  = ( location >= 0 && location + numBytes <= size( mems[ 0 ].\data ) );

public int memorySize( store( a, b, list[meminst] mems, c ) ) = size( mems[ 0 ].\data ) / ( 64 * 1024 );

public Maybe[store] memoryGrow( store( a, b, list[meminst] mems, c ), int numNewPages )
  = just( store( a, b, setAt( mems, 0, memoryGrow( mems[ 0 ], numNewPages ) ), c ) )
  when canGrowMemory( mems[ 0 ], numNewPages );
public default Maybe[store] memoryGrow( store( a, b, list[meminst] mems, c ), int numNewPages )
  = nothing( );

private meminst memoryGrow( meminst( bytes \data, int maxNumPages ), int numNewPages )
  = meminst( \data + [ 0 | i <- [0..numNewPages*PAGESIZE] ], maxNumPages );
private meminst memoryGrow( meminst( bytes \data ), int numNewPages )
  = meminst( \data + [ 0 | i <- [0..numNewPages*PAGESIZE] ] );

private bool canGrowMemory( meminst( bytes \data, int maxNumPages ), int numNewPages )
  = ( currNumPages + numNewPages <= min( MAX_NUM_PAGES, maxNumPages ) )
  when currNumPages := size( \data ) / PAGESIZE;
private bool canGrowMemory( meminst( bytes \data ), int numNewPages )
  = ( currNumPages + numNewPages <= MAX_NUM_PAGES ) // No max was set
  when currNumPages := size( \data ) / PAGESIZE;

private list[meminst] setupMeminsts( list[IMPORT] importMems, list[MEM] mems, list[DATA] \data ) {
  insts = [ emptyMeminst( ) | i <- importMems ] + [ toMeminst( m ) | m <- mems ];
  for ( d <- \data ) {
    insts = applyDataToMeminst( insts, d ); 
  }
  return insts;
}
private meminst emptyMeminst( ) = meminst( [ 0 | i <- [0..64*1024] ] );
private meminst toMeminst( mem( memtype( limits( int min, int max ) ) ) ) = meminst( [ 0 | i <- [0..min*(64*1024)] ], max );
private meminst toMeminst( mem( memtype( limits( int min ) ) ) ) = meminst( [ 0 | i <- [0..min*(64*1024)] ] );

private list[meminst] applyDataToMeminst( list[meminst] insts, \data( MEMIDX idx, EXPR offset, list[byte] init ) ) {
  int offsetVal = evaluate( offset ).ival;
  meminst inst = insts[ idx ];
  
  // Constraints on the initialisation of memory sections are not fully
  // specified. So, for now, no constraints are enforced or assumed.
  // An exception about too little memory being allocated may be thrown
  // because of this.
  
  for ( i <- [0..size(init)] ) {
    inst.\data[ offsetVal + i ] = init[ i ];
  }
  
  insts[ idx ] = inst;
  return insts;
}

// ## Global
public store setGlobal( store( funcs, tables, mems, globals ), int id, runtime_val val ) {
  globals[ id ].val = val;
  return store( funcs, tables, mems, globals );
}
public runtime_val getGlobal( store( _, _, _, globals ), int id ) = globals[ id ].val;

private  list[globalinst] toGlobalinsts( list[GLOBAL] globals ) = [ toGlobalinst( g ) | g <- globals ];
private globalinst toGlobalinst( g:global( globaltype( MUT mut, VALTYPE vt ), EXPR e ) ) = globalinst( evaluate( e ), mut );

// ## Tables
private list[tableinst] setupTableinsts( list[IMPORT] importTables, list[TABLE] tables, list[ELEM] elems ) {
  insts = [ emptyTableinst( ) | t <- importTables ] + [ setupTableinst( t ) | t <- tables ];
  for ( e <- elems ) {
    insts = applyElemToTableinst( insts, e ); 
  }
  return insts;
}
private tableinst emptyTableinst( ) = tableinst( [] );
private tableinst setupTableinst( table( tabletype( limits( min, max ), elemType ) ) ) = tableinst( [], max );
private tableinst setupTableinst( table( tabletype( limits( min ), elemType ) ) ) = tableinst( [] );

private list[tableinst] applyElemToTableinst( list[tableinst] insts, elem( TABLEIDX idx, EXPR offset, list[FUNCIDX] init ) ) {
  int offsetVal = evaluate( offset ).ival;
  tableinst inst = insts[ idx ];
  
  if ( tableinst( _, int max ) := inst && offsetVal + size( init ) > max ) {
    throw AssertionFailed( "Table capacity exceeded" );
  }
  
  if ( size( inst.elem ) < offsetVal + size( init ) ) {
    inst.elem += [ funcelem( ) | i <- [0..(offsetVal + size( init ) - size( inst.elem ))] ];
  }
  
  for ( i <- [0..size( init )] ) {
    inst.elem[ offsetVal + i ] = funcelem( init[ i ] );
  }
  insts[ idx ] = inst;
  return insts;
}

// ## Stack
public moduleinst getModuleInst( Stack s ) = getModuleInst( getFrame( s ) );

private Stack updateFrame( [*S, sef( int retArity, frame f )], frame(frame) fUpdateFrame ) = [*S, sef( retArity, fUpdateFrame( f ) ) ];
private Stack updateFrame( [*S, v], frame(frame) fUpdateFrame ) = updateFrame( S, fUpdateFrame ) + v;
private Stack updateFrame( [ ], frame(frame) fUpdateFrame ) = \throw( AssertionFailed( "There must be at least one frame on the stack" ) );

private frame getFrame( [*S, sef( int retArity, frame f )] ) = f;
private frame getFrame( [*S, _] ) = getFrame( S );
private frame getFrame( [] ) = \throw( AssertionFailed( "There must be at least one frame on the stack" ) );

public runtime_val getLocal( Stack s, int id ) = getLocal( getFrame( s ), id );
private runtime_val getLocal( frame( list[runtime_val] locals, _ ), int id ) = locals[ id ];

public Stack setLocal( Stack s, int id, runtime_val val ) = updateFrame( s, frame (frame f) { return setLocal( f, id, val ); } );
private frame setLocal( frame( list[runtime_val] locals, moduleinst m ), int id, runtime_val val ) = frame( setAt( locals, id, val ), m );

// ## Helpers

public int arity( resulttype( list[VALTYPE] results ) ) = size( results );

public void \throw( RuntimeException ex ) {
  throw ex;
}

private list[&T] setAt( list[&T] dst, int offset, &T val ) {
  dst[offset] = val;
  return dst;
}

private list[&T] mergeAt( list[&T] dst, int offset, list[&T] src ) {
  for ( int i <- [0..size(src)] ) {
    dst[ offset + i ] = src[ i ];
  }
  return dst;
}

private moduleinst getModuleInst( frame( list[runtime_val] locals, moduleinst \module ) ) = \module;

private runtime_val evaluate( expr( [ i32_const( int ival ) ] ) ) = i32( ival );
private runtime_val evaluate( expr( [ i64_const( int ival ) ] ) ) = i64( ival );
private runtime_val evaluate( expr( [ f32_const( Float fval ) ] ) ) = f32( fval );
private runtime_val evaluate( expr( [ f64_const( Float fval ) ] ) ) = f64( fval );
private runtime_val evaluate( expr( [ get_global( _ ) ] ) ) = i32( 0 );
private runtime_val evaluate( expr( [ nop( ) ] ) ) = i32( 0 );

public runtime_val runtimeInit( VALTYPE t:i32( ) ) = i32( 0 );
public runtime_val runtimeInit( VALTYPE t:i64( ) ) = i64( 0 );
public runtime_val runtimeInit( VALTYPE t:f32( ) ) = f32( Float_zero( ) );
public runtime_val runtimeInit( VALTYPE t:f64( ) ) = f64( Float_zero( ) );
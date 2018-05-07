module lang::webassembly::Execution

import lang::webassembly::ADT;
import List;
import util::Math;
import IO; // temp

config execStep( c:config( store s, thread( frame f, list[INSTR] instrs ) ) )
  = config( s, thread( f, reduce( instrs ) ) );
  
// TODO: Make sure it is done once the function's result is obtained
bool isDone( config( store s, thread( frame f, [] ) ) ) = true;
default bool isDone( config c ) = false;

list[INSTR] reduce( [i32_const( ival1 ), i32_const( ival2 ), i32_add( ), *L] )
  = [ i32_const( ( ival1 + ival2 ) % toInt( pow( 2, 32 ) ) ), *L ];
list[INSTR] reduce( [i64_const( ival1 ), i64_const( ival2 ), i64_add( ), *L] )
  = [ i64_const( ( ival1 + ival2 ) % toInt( pow( 2, 64 ) ) ), *L ];
list[INSTR] reduce( [ i32_const( _ ), drop( ), *L ] ) = L;
list[INSTR] reduce( [ i64_const( _ ), drop( ), *L ] ) = L;
list[INSTR] reduce( [ f32_const( _ ), drop( ), *L ] ) = L;
list[INSTR] reduce( [ f64_const( _ ), drop( ), *L ] ) = L;

// This is an auxiliary rule to accomodate for executing instructions further from the
//   top of the stack, if no valid instruction is currently on top.
default list[INSTR] reduce( [A, *L] ) = [ A, *reduce( L ) ];
default list[INSTR] reduce( list[INSTR] instrs ) {
  println( "reduce rule not implemented" );
  return [];
}

data runtime_val = rt_i32_const( int ival )
                 | rt_i64_const( int ival )
                 | rt_f32_const( real fval )
                 | rt_f64_const( real fval )
                 ;

// The store represents all global state that can be manipulated by WebAssembly
// programs. It consists of the runtime representation of all instances of
// functions, tables, memories, and globals that have been allocated during the
// life time of the abstract machine.
data store = store( list[funcinst] funcs, list[tableinst] tables, list[meminst] mems, list[globalinst] globals );

alias funcaddr = int;
alias tableaddr = int;
alias memaddr = int;
alias globaladdr = int;

data moduleinst = moduleinst( list[FUNCTYPE] types,
                              list[funcaddr] funcaddrs,
                              list[tableaddr] tableaddrs,
                              list[memaddr] memaddrs,
                              list[globaladdr] globaladdrs,
                              list[exportinst] exports );

// TODO?: funcinst should also include host funcs, which are imported.
//        not sure how that would work here. Left it out for now.
data funcinst = funcinst( FUNCTYPE \type, moduleinst \module, FUNC code );

data tableinst = tableinst( /* TODO */ );

data meminst = meminst( /* TODO */ );

data globalinst = globalinst( /* TODO */ );

data exportinst = exportinst( /* TODO */ );

// TODO: External Values

alias Stack = list[stackelem];

data stackelem = stackelem_val( runtime_val val )
               | stackelem_label( /* TODO */ )
               | stackelem_activation( frame f )
               ;

data frame = frame( list[runtime_val] locals, moduleinst \module );

data config = config( store s, thread t );

data thread = thread( frame f, list[INSTR] instrs );

// Sets up an execution config to the module's start function
//   Note: Modules without a start function are not supported (for now)
config setupExecutionConfig( m:\module( types, list[FUNC] funcs, tables, mems, globals, elems, \data, START st, imports, exports ) ) {
  FUNC startFunc = funcs[ getStartFuncIdx( st ) ];
  moduleinst mInst = moduleinst( types, [0..size(funcs)], [0..size(tables)], [0..size(mems)], [0..size(globals)], [ /* TODO */ ] );
  frame f = frame( setupFuncLocals( types, startFunc ), mInst );
  store s = store( [], [], [], [] ); // TODO
  thread t = thread( f, getFuncInstrs( startFunc ) );
  return config( s, t );
}

list[INSTR] getFuncInstrs( func( _, _, expr( instrs ) ) ) = instrs;

FUNCIDX getStartFuncIdx( \start( FUNCIDX funcIdx ) ) = funcIdx;

list[runtime_val] setupFuncLocals( list[FUNCTYPE] types, func( TYPEIDX tIdx, list[VALTYPE] locals, EXPR body ) )
  = setupLocalsFromParams( types[ tIdx ] ) + [ initLocal( localType ) | localType <- locals ];

list[runtime_val] setupLocalsFromParams( functype( list[VALTYPE] params, _ ) ) = [ initLocal( paramType ) | paramType <- params ];
runtime_val initLocal( i32( ) ) = rt_i32_const( 0 );
runtime_val initLocal( i64( ) ) = rt_i64_const( 0 );
runtime_val initLocal( f32( ) ) = rt_f32_const( 0.0 );
runtime_val initLocal( f64( ) ) = rt_f64_const( 0.0 );
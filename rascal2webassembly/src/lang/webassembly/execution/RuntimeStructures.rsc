module lang::webassembly::execution::RuntimeStructures

import lang::webassembly::Abstract;
import lang::webassembly::execution::Numerics;
import util::Float;

// ## Runtime structures

// The integer values are in their unsigned representation
//   Their internal representation is agnostic to signedness, this signedness
//   will be enforced by the operations performed upon them.
data runtime_val = i32( int ival )
                 | i64( int ival )
                 | f32( Float fval )
                 | f64( Float fval )
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

data funcinst = funcinst( FUNCTYPE \type, moduleinst \module, FUNC code )
              | funcinst( FUNCTYPE \type, str hostModule, str hostFunc )
              ;

data tableinst = tableinst( list[funcelem] elem, int max )
               | tableinst( list[funcelem] elem )
               ;

data funcelem = funcelem( funcaddr addr )
              | funcelem( )
              ;

data meminst = meminst( bytes \data, int maxNumPages )
             | meminst( bytes \data )
             ;

data globalinst = globalinst( runtime_val val, MUT mut );

data exportinst = exportinst( str name, externval val );

data externval = externval_func( funcaddr func )
               | externval_table( tableaddr table )
               | externval_mem( memaddr mem )
               | externval_global( globaladdr global )
               ;

data CONTROL_INSTR = trap( )
                   | invoke( int addr )
                   | end( ) // pseudo instruction
                   ;

// 'se' stands for 'stackelem'. Shortened for readability because they occur a lot
data stackelem = sev( runtime_val val )
               | sel( int retArity, list[INSTR] instrs ) // label
               | sef( int retArity, frame f ) // frame
               ;
               
data instrelem = sei( INSTR instr ) // instruction
               | sec( CONTROL_INSTR cInstr ) // control instruction
               ;

data frame = frame( list[runtime_val] locals, moduleinst \module );

alias Stack = list[stackelem];

// Deviation from format spec to account for the explicit stack.
data thread = thread( Stack stack, list[instrelem] instructions );

data config = config( store s, thread t );

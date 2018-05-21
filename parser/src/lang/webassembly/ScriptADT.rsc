module lang::webassembly::ScriptADT

extend lang::webassembly::ADT;
import lang::webassembly::Float;

// This structure is not formally defined, instead it is used in the
//   testsuite and the reference parser. A subset of those instructions
//   is implemented here. (e.g. binary module conversion tests
//   and validation tests are excluded)

data WASM_SCRIPT = script_module( list[WASM_SCRIPT_ENTRY] entries );

data WASM_SCRIPT_ENTRY = script_entry( MODULE \module, list[SCRIPT_LINE] lines );

data SCRIPT_LINE = assertion( ASSERTION assertion )
                 | action( ACTION action )
                 ;

data ASSERTION = assert_return( ACTION action, list[CONST] vals )
               | assert_return_canonical_nan( ACTION action )
               | assert_return_arithmetic_nan( ACTION action )
               | assert_trap( ACTION action )
               | assert_exhaustion( ACTION action )
               ;

data CONST = CONST_i32( int ival )
           | CONST_i64( int ival )
           | CONST_f32( Float fval )
           | CONST_f64( Float fval )
           ;

data ACTION = invoke( str name, list[CONST] arguments )
            | get( str name )
            ;

module lang::webassembly::script::ScriptSyntax2AST

import String;
import IO;
import util::Maybe;

import lang::webassembly::Syntax2AST;
import lang::webassembly::script::ScriptAbstract;
import lang::webassembly::script::ScriptSyntax;
import lang::webassembly::execution::Numerics; // for invSigned( )
import lang::webassembly::ToFloat;
import util::Float;

WASM_SCRIPT toAST( start[WebAssemblyScript] script ) {
  list[WASM_SCRIPT_ENTRY] entries = [];
  
  Maybe[MODULE] \module = nothing( );
  int numModules = 0;
  map[str,int] moduleNames = ( );
  list[SCRIPT_LINE] moduleLines = [];
  
  top-down-break visit ( script ) {
  case (ScriptModule)`<Module m>`: {
    if ( just( prevM ) := \module ) {
      entries += script_entry( prevM, moduleLines );
    }
    if ( (Module)`(module <Id? id> <ModuleField* _>)` := m && "<id>" != "" ) {
      moduleNames[ "<id>" ] = numModules;
    }
    \module = just( toAST( m ) );
    moduleLines = [];
    numModules += 1;
  }
  case ScriptModule m: {
    if ( just( prevM ) := \module ) {
      entries += script_entry( prevM, moduleLines );
    }
    \module = nothing( );
  }
  case Assertion a: {
    if ( just( prevM ) := \module && just( <assertionAdt,modName> ) := toAST( a ) ) {
      if ( modName != "" ) {
        int modId = moduleNames[ modName ];
        if ( modId + 1 == numModules ) {
        moduleLines += assertion( assertionAdt );
        } else {
          entries[ modId ].lines = entries[ modId ].lines + assertion( assertionAdt );
        }
      } else {
        moduleLines += assertion( assertionAdt );
      }
    }
  }
  case Action a: {
    if ( just( prevM ) := \module ) {
      <aAdt, modName> = toAST( a );
      if ( modName != "" ) {
        int modId = moduleNames[ modName ];
        if ( modId + 1 == numModules ) {
          moduleLines += action( aAdt );
        } else {
          entries[ modId ].lines = entries[ modId ].lines + action( aAdt );
        }
      } else {
        moduleLines += action( aAdt );
      }
    }
  }
  // Actions and registers are ignored
  }
  
  if ( just( prevM ) := \module ) {
    entries += script_entry( prevM, moduleLines );
  }
  
  return script_module( entries );
}

Maybe[tuple[ASSERTION,str]] toAST( (Assertion)`(assert_return <Action ac> <Const* consts>)` )
  = just( <assert_return( acADT, [ toAST( c ) | c <- consts ] ), modName> )
  when <acADT,modName> := toAST( ac );
Maybe[tuple[ASSERTION,str]] toAST( (Assertion)`(assert_return_canonical_nan <Action ac>)` )
  = just( <assert_return_canonical_nan( acADT ), modName> )
  when <acADT,modName> := toAST( ac );
Maybe[tuple[ASSERTION,str]] toAST( (Assertion)`(assert_return_arithmetic_nan <Action ac>)` )
  = just( <assert_return_arithmetic_nan( acADT ), modName> )
  when <acADT,modName> := toAST( ac );
Maybe[tuple[ASSERTION,str]] toAST( (Assertion)`(assert_exhaustion <Action ac> <String s>)` )
  = just( <assert_exhaustion( acADT ), modName> )
  when <acADT,modName> := toAST( ac );
Maybe[tuple[ASSERTION,str]] toAST( (Assertion)`(assert_trap <Action ac> <String s>)` )
  = just( <assert_trap( acADT ), modName> )
  when <acADT,modName> := toAST( ac );
Maybe[tuple[ASSERTION,str]] toAST( Assertion a ) = nothing( );

tuple[ACTION action,str modName] toAST( (Action)`(invoke <Id? id> <Name name> <Const* consts>)` ) = <invoke( toAST( name ), [ toAST( c ) | c <- consts ] ), "<id>" >;
tuple[ACTION action,str modName] toAST( (Action)`(get <Id? id> <Name name>)` ) = <get( toAST( name ) ), "<id>">;

CONST toAST( (Const)`(i32.const <IN i>)` ) = CONST_i32( invSigned( 32, toInt( "<i>" ) ) );
CONST toAST( (Const)`(i64.const <IN i>)` ) = CONST_i64( invSigned( 64, toInt( "<i>" ) ) );
CONST toAST( (Const)`(f32.const <FN f>)` ) = CONST_f32( toFloat( "<f>" ) );
CONST toAST( (Const)`(f64.const <FN f>)` ) = CONST_f64( toFloat( "<f>" ) );

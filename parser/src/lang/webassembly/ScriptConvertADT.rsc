module lang::webassembly::ScriptConvertADT

import String;
import IO;
import util::Maybe;

import lang::webassembly::ConvertADT;
import lang::webassembly::ScriptADT;
import lang::webassembly::ScriptSyntax;
import lang::webassembly::execution::Numerics; // for invSigned( )
import lang::webassembly::ToFloat;
import util::Float;

WASM_SCRIPT toADT( start[WebAssemblyScript] script ) {
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
    \module = just( toADT( m ) );
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
    if ( just( prevM ) := \module && just( <assertionAdt,modName> ) := toADT( a ) ) {
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
      <aAdt, modName> = toADT( a );
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

Maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_return <Action a> <Const* consts>)` )
  = just( <assert_return( aADT, [ toADT( c ) | c <- consts ] ), modName> )
  when <aADT,modName> := toADT( a );
Maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_return_canonical_nan <Action a>)` )
  = just( <assert_return_canonical_nan( aADT ), modName> )
  when <aADT,modName> := toADT( a );
Maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_return_arithmetic_nan <Action a>)` )
  = just( <assert_return_arithmetic_nan( aADT ), modName> )
  when <aADT,modName> := toADT( a );
Maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_exhaustion <Action a> <String s>)` )
  = just( <assert_exhaustion( aADT ), modName> )
  when <aADT,modName> := toADT( a );
Maybe[tuple[ASSERTION,str]] toADT( Assertion a ) = nothing( );

tuple[ACTION action,str modName] toADT( (Action)`(invoke <Id? id> <Name name> <Const* consts>)` ) = <invoke( toADT( name ), [ toADT( c ) | c <- consts ] ), "<id>" >;
tuple[ACTION action,str modName] toADT( (Action)`(get <Id? id> <Name name>)` ) = <get( toADT( name ) ), "<id>">;

CONST toADT( (Const)`(i32.const <IN i>)` ) = CONST_i32( invSigned( 32, toInt( "<i>" ) ) );
CONST toADT( (Const)`(i64.const <IN i>)` ) = CONST_i64( invSigned( 64, toInt( "<i>" ) ) );
CONST toADT( (Const)`(f32.const <FN f>)` ) = CONST_f32( toFloat( "<f>" ) );
CONST toADT( (Const)`(f64.const <FN f>)` ) = CONST_f64( toFloat( "<f>" ) );

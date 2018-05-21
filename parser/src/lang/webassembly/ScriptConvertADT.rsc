module lang::webassembly::ScriptConvertADT

import lang::webassembly::ConvertADT;
import lang::webassembly::ScriptADT;
import lang::webassembly::ScriptSyntax;
import lang::webassembly::Operations; // for invSigned()
import lang::webassembly::Float;
import String;

import IO;

WASM_SCRIPT toADT( start[WebAssemblyScript] script ) {
  list[WASM_SCRIPT_ENTRY] entries = [];
  
  maybe[MODULE] \module = empty( );
  int numModules = 0;
  map[str,int] moduleNames = ( );
  list[SCRIPT_LINE] moduleLines = [];
  
  top-down-break visit ( script ) {
  case (ScriptModule)`<Module m>`: {
    if ( ok( prevM ) := \module ) {
      entries += script_entry( prevM, moduleLines );
    }
    if ( (Module)`(module <Id? id> <ModuleField* _>)` := m && "<id>" != "" ) {
      moduleNames[ "<id>" ] = numModules;
    }
    \module = ok( toADT( m ) );
    moduleLines = [];
    numModules += 1;
  }
  case ScriptModule m: {
    if ( ok( prevM ) := \module ) {
      entries += script_entry( prevM, moduleLines );
    }
    \module = empty( );
  }
  case Assertion a: {
    if ( ok( prevM ) := \module && ok( <assertionAdt,modName> ) := toADT( a ) ) {
      if ( modName != "" ) {
        int modId = moduleNames[ modName ];
        if ( modId + 1 == numModules ) {
        moduleLines += assertion( assertionAdt );
        } else {
          println( entries[ modId ].lines );
          entries[ modId ].lines = entries[ modId ].lines + assertion( assertionAdt );
        }
      } else {
        moduleLines += assertion( assertionAdt );
      }
    }
  }
  case Action a: {
    if ( ok( prevM ) := \module ) {
      <aAdt, modName> = toADT( a );
      if ( modName != "" ) {
        int modId = moduleNames[ modName ];
        if ( modId + 1 == numModules ) {
          moduleLines += action( aAdt );
        } else {
          println( entries[ modId ].lines );
          entries[ modId ].lines = entries[ modId ].lines + action( aAdt );
        }
      } else {
        moduleLines += action( aAdt );
      }
    }
  }
  // Actions and registers are ignored
  }
  
  if ( ok( prevM ) := \module ) {
    entries += script_entry( prevM, moduleLines );
  }
  
  println( moduleNames );
  
  return script_module( entries );
}

data maybe[&T] = ok( &T val ) | empty( );

maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_return <Action a> <Const* consts>)` )
  = ok( <assert_return( aADT, [ toADT( c ) | c <- consts ] ), modName> )
  when <aADT,modName> := toADT( a );
maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_return_canonical_nan <Action a>)` )
  = ok( <assert_return_canonical_nan( toADT( a ) ), modName> )
  when <aADT,modName> := toADT( a );
maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_return_arithmetic_nan <Action a>)` )
  = ok( <assert_return_arithmetic_nan( toADT( a ) ), modName> )
  when <aADT,modName> := toADT( a );
maybe[tuple[ASSERTION,str]] toADT( (Assertion)`(assert_exhaustion <Action a> <String s>)` )
  = ok( <assert_exhaustion( toADT( a ) ), modName> )
  when <aADT,modName> := toADT( a );
maybe[tuple[ASSERTION,str]] toADT( Assertion a ) = empty( );

tuple[ACTION action,str modName] toADT( (Action)`(invoke <Id? id> <Name name> <Const* consts>)` ) = <invoke( toADT( name ), [ toADT( c ) | c <- consts ] ), "<id>" >;
tuple[ACTION action,str modName] toADT( (Action)`(get <Id? id> <Name name>)` ) = <get( toADT( name ) ), "<id>">;

CONST toADT( (Const)`(i32.const <IN i>)` ) = CONST_i32( invSigned( toInt( "<i>" ), 32 ) );
CONST toADT( (Const)`(i64.const <IN i>)` ) = CONST_i64( invSigned( toInt( "<i>" ), 64 ) );
CONST toADT( (Const)`(f32.const <FN f>)` ) = CONST_f32( toFloat( "<f>" ) );
CONST toADT( (Const)`(f64.const <FN f>)` ) = CONST_f64( toFloat( "<f>" ) );

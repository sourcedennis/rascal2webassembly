module HelpersWasm

import lang::webassembly::Desugar;
import lang::webassembly::Syntax;
import lang::webassembly::script::ScriptSyntax;
import lang::webassembly::execution::RuntimeStructures;

import ParseTree;
import List;

start[WebAssembly] parseWasm( str s ) = parse( #start[WebAssembly], s );
start[WebAssembly] parseWasm( str s, loc l ) = parse( #start[WebAssembly], s, l );
start[WebAssemblyScript] parseWasmScript( loc l ) = parse( #start[WebAssemblyScript], l );
start[WebAssemblyScript] parseWasmScript( str s ) = parse( #start[WebAssemblyScript], s );
start[WebAssemblyScript] parseWasmScript( str s, loc l ) = parse( #start[WebAssemblyScript], s, l );

public str stack2str( Stack s ) = "[" + intercalate( ", ", [ stack2str( v ) | v <- s ] ) + "]";

private str stack2str( sev( v ) ) = "<v>";
private str stack2str( sel( retArity, _ ) ) = "#label(<retArity>)";
private str stack2str( sef( retArity, frame( locals, _ ) ) ) = "frame(<retArity>,<locals>)";

public str instrs2str( list[instrelem] instrs ) = "[" + intercalate( ", ", [ instr2str( v ) | v <- instrs ] ) + "]";

private str instr2str( sei( i ) ) = "<i>";
private str instr2str( sec( i ) ) = "<i>";

start[WebAssemblyScript] desugar( start[WebAssemblyScript] s )
  = visit( s ) {
  case Module m => desM when (start[WebAssembly])`<Module desM>` := desugar( (start[WebAssembly])`<Module m>` )
  };
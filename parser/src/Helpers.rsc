module Helpers

import lang::webassembly::Syntax;
import lang::webassembly::ScriptSyntax;
import lang::webassembly::execution::RuntimeStructures;
import List;

start[WebAssembly] parseWasm( str s ) = parse( #start[WebAssembly], s );
start[WebAssemblyScript] parseWasmScript( loc l ) = parse( #start[WebAssemblyScript], l );
start[WebAssemblyScript] parseWasmScript( str s ) = parse( #start[WebAssemblyScript], s );

public str stack2str( Stack s ) = "[" + intercalate( ", ", [ stack2str( v ) | v <- s ] ) + "]";

private str stack2str( sev( v ) ) = "<v>";
private str stack2str( sel( retArity, _ ) ) = "#label(<retArity>)";
private str stack2str( sef( retArity, frame( locals, _ ) ) ) = "frame(<retArity>,<locals>)";